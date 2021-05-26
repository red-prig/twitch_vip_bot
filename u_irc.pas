unit u_irc;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils,
 sockets,
 mtRandom,
 ulog,
 nghttp2,
 fphttp1,
 UJson,
 fpURI,
 fpwebsocket,
 evpool,bufferevent_openssl,openssl;

type
 THttpClient=class;

 THttpStream=class
  FClientData:THttpClient;
  Method,Host,Path:RawByteString;
  headers:array of RawByteString;
  status:RawByteString;
  stream_id:int32;
  FSends,FRecvs:TStream;
  FOnEndStream:TNotifyEvent;
  Constructor Create;
  Procedure SetUrl(const url:RawByteString);
  Procedure AddHeader(Const name,value:RawByteString);
  Procedure AddStdHdr;
  procedure on_begin_data; virtual;
  procedure on_begin_headers(cat:Longint); virtual;
  procedure on_recv_data(data:pointer;len:size_t); virtual;
  procedure on_headers(Const name,value:RawByteString;cat:Longint); virtual;
  procedure on_end_headers; virtual;
  procedure on_end_stream; virtual;
 end;

type
 THttpStream2Mem=class(THttpStream)
  Constructor Create;
  Destructor  Destroy; override;
 end;

 TProgInfo=record
  Load,Size,Speed:QWORD;
 end;

 TProgXCHG=object
  private
   type
    PBlock=^TBlock;
    TBlock=record
     FTime:QWord;
     Load,Size:QWORD;
    end;
   Var
    FA,FB:TBlock;
    PFA,PFB:PBlock;
    FTime:QWord;
    Prev:TProgInfo;
  public
   Procedure Clear;
   Procedure Write(_Load,_Size:QWORD);
   function  Read(Offset:QWORD):TProgInfo;
 end;

 THttpStream2File=class(THttpStream)
  FLen:QWORD;
  Prog:TProgXCHG;
  procedure   on_headers(Const name,value:RawByteString;cat:Longint); override;
  procedure   on_recv_data(data:pointer;len:size_t); override;
  Constructor Create(const AFileName:string;Mode:Word); reintroduce;
  Destructor  Destroy; override;
 end;

 THttpClientClass=class of THttpClient;

 THttpClient=class
  //private
   bev:Pbufferevent;
   session:pnghttp2_session;
   http2:Boolean;
   FQueue:TFPList;
  //public
  Constructor Create_hostname(ssl_ctx:Pssl_ctx;family:Integer;hostname:PAnsiChar;port:Word);

  Destructor Destroy; override;

  function  get_stream_user_data(stream_id:PtrUInt):THttpStream;
  procedure set_stream_user_data(stream_id:PtrUInt;stream_data:THttpStream);
  procedure delete_stream_data(stream_data:THttpStream); virtual;

  function  stream_get_state(stream_id:PtrUInt):Tnghttp2_stream_proto_state;

  function  ALPN:Boolean;
  procedure NewSession1;
  procedure NewSession2;
  function  session_connect:integer; virtual;
  function  session_send:integer;
  function  session_recv:integer;
  procedure session_shutdown(how:Longint);
  procedure submit_request(stream_data:THttpStream;nva:Pnghttp2_nv;nvlen:size_t);
  procedure submit(stream_data:THttpStream);
  procedure terminate;
  procedure print_err;
 end;

function  replyConnect(var ClientData:THttpClient;AClass:THttpClientClass;Const Path:RawByteString):Boolean;

procedure reply_irc_Connect(const login,oAuth,chat:RawByteString);
procedure reply_irc_Disconnect;
procedure reply_irc_msg(const msg:RawByteString);
procedure reply_irc_reconnect;
procedure reply_irc_Connect2(const login,oAuth,chat:RawByteString);
procedure reply_irc_msg2(const msg:RawByteString);

function UIDToString(const GUID: TGUID): RawByteString;
function TryStringToUID(const S: RawByteString; out Guid: TGUID): Boolean;

implementation

uses
 Main;

type
 irc_log=class(app_logic);

var
 wss_ctx:PSSL_CTX;
 https_ctx:PSSL_CTX;
 msg_send_buf:Pevbuffer;
 msg_send_buf2:Pevbuffer;

 http1_callbacks,http2_callbacks:pnghttp2_session_callbacks;

Const
 SSL_TLSEXT_ERR_OK = 0;
 SSL_TLSEXT_ERR_ALERT_WARNING = 1;
 SSL_TLSEXT_ERR_ALERT_FATAL = 2;
 SSL_TLSEXT_ERR_NOACK = 3;

Const
 Protos_NPN:string=#8'http/1.1';
 Protos_ALPN1:string=#8'http/1.1'{#2'h2'};
 Protos_ALPN2:string=#8'http/1.1'#2'h2';

Function GetStr(data:Pchar;size:size_t):String; inline;
begin
 SetString(Result,data,size);
end;

Function GetALPN(_in:PByte;inlen:ptrUInt):String;
Var
 i:ptrUInt;
begin
 i:=0;
 Result:='';
 While (i<=inlen) do
 begin
  if (i+ptrUInt(_in[i])>inlen) then Break;
  Result:=Result+'#'+IntToStr(_in[i]);
  Result:=Result+''''+GetStr(PChar(@_in[i+1]),_in[i])+'''';
  i:=i+ptrUInt(_in[i]+1)
 end;
end;

function next_proto_cb(ssl:PSSL;Var data:Pointer;Var len:size_t;_in:Pointer;arg:Pointer):Integer; cdecl;
begin
 data:=PChar(Protos_NPN);
 len:=Length(Protos_NPN);
 Log(irc_log,0,['NPN:',GetALPN(data,len)]);
 Result:=SSL_TLSEXT_ERR_OK;
end;

function select_next_protocol(var _out:PByte;var outlen:byte;
                             _in:PByte;inlen:ptrUInt;
                             key:PChar;keylen:ptrUInt):ptrInt;
Var
 i:ptrUInt;
begin
 i:=0;
 While (i+keylen<=inlen) do
 begin
  if (CompareByte(_in[i],key^,keylen)=0) then
  begin
   _out  :=@_in[i+1];
   outlen:= _in[i];
   Exit(0);
  end;
  i:=i+ptrUInt(_in[i]+1)
 end;
 Exit(-1);
end;

function select_next_protocol(var _out:PByte;var outlen:byte;
                             _in:PByte;inlen:ptrUInt):ptrInt;
begin
 if select_next_protocol(_out,outlen,_in,inlen,NGHTTP2_PROTO_ALPN,
                         NGHTTP2_PROTO_ALPN_LEN)=0 then
 begin
  Exit(1);
 end;
 if select_next_protocol(_out,outlen,_in,inlen,NGHTTP2_HTTP_1_1_ALPN,
                         NGHTTP2_HTTP_1_1_ALPN_LEN)=0 then
 begin
  Exit(1);
 end;
 Exit(-1);
end;

function alpn_select_proto_cb(ssl:PSSL;Var _out:Pointer;Var outlen:Byte;_in:Pointer;inlen:DWORD;arg:Pointer):Integer; cdecl;
begin
 //Writeln('ALPN:',GetStr((_in)+1,inlen-1),':',GetStr(_out,outlen));
 {if nghttp2_select_next_protocol(_out,@outlen,_in,inlen)<>0 then
 begin
  Exit(SSL_TLSEXT_ERR_NOACK);
 end;}

 if select_next_protocol(PByte(_out),outlen,_in,inlen)<0 then
 begin
  Exit(SSL_TLSEXT_ERR_NOACK);
 end;

 //_out:=HTTP_1_1_ALPN;
 //outlen:=Length(HTTP_1_1_ALPN);
 Log(irc_log,0,['ALPN:',GetALPN(_in,inlen),':',GetStr(_out,outlen)]);
 Exit(SSL_TLSEXT_ERR_OK);
end;

Const
 Crypt32='Crypt32.dll';

type
 HCERTSTORE=THandle;

 PCERT_INFO=Pointer;
 TCERT_CONTEXT=packed record
  dwCertEncodingType:PTRUINT;
  pbCertEncoded:PByte;
  cbCertEncoded:PTRUINT;
  pCertInfo:PCERT_INFO;
  hCertStore:HCERTSTORE;
 end;
 PCCERT_CONTEXT=^TCERT_CONTEXT;

function CertOpenSystemStoreW(hProv:THandle;szSubsystemProtocol:PWideChar):HCERTSTORE; stdcall; external Crypt32;
function CertEnumCertificatesInStore(_hCertStore:HCERTSTORE;pPrevCertContext:PCCERT_CONTEXT):PCCERT_CONTEXT; stdcall; external Crypt32;
function CertCloseStore(_hCertStore:HCERTSTORE;dwFlags:DWORD):LongBool; stdcall; external Crypt32;

function SSL_CTX_load_store(ctx:PSSL_CTX):Boolean;
var
 store:PX509_STORE;
 count:Integer=0;

 procedure _load_StoreW(szSubsystemProtocol:PWideChar);
 var
  hStore:HCERTSTORE;
  pContext:PCCERT_CONTEXT=nil;
  x509:Px509;
 begin
  hStore:=CertOpenSystemStoreW(0,szSubsystemProtocol);
  if (hStore=0) then Exit;
  while true do
  begin
   pContext:=CertEnumCertificatesInStore(hStore,pContext);
   if (pContext=nil) then Break;
   x509:=d2i_X509(nil,@pContext^.pbCertEncoded,pContext^.cbCertEncoded);
   if (x509<>nil) then
   begin
    if X509_STORE_add_cert(store,x509)=1 then Inc(count);
    X509_free(x509);
   end;
  end;
  CertCloseStore(hStore,0);
 end;

begin
 store:=X509_STORE_new();
 _load_StoreW('ROOT');
 _load_StoreW('CA');
 Result:=(count<>0);
 if Result then
 begin
  SSL_CTX_set_cert_store(ctx,store);
 end else
 begin
  X509_STORE_free(store);
 end;
end;

Function create_ssl_ctx(support_http2:Boolean):PSSL_CTX;
Var
 M:PSSL_METHOD;
begin
 M:=SSLv23_client_method();
 if not Assigned(M) then
 begin
  M:=TLS_client_method();
 end;
 Result:=SSL_CTX_new(M);

 if not Assigned(Result) then
 begin
  Log(irc_log,1,['Could not create SSL/TLS context: ',ERR_error_string(ERR_get_error(),nil)]);
  Exit;
 end;

 SSL_CTX_set_next_protos_advertised_cb(Result,Tnext_proto_advertised_cb(@next_proto_cb),nil);
 SSL_CTX_set_next_proto_select_cb(Result,Tnext_proto_select_cb(@alpn_select_proto_cb),nil);
 SSL_CTX_set_alpn_select_cb(Result,Tnext_proto_select_cb(@alpn_select_proto_cb),nil);

 Case support_http2 of
  False:SSL_CTX_set_alpn_protos(Result,PByte(PChar(Protos_ALPN1)),Length(Protos_ALPN1));
  True :SSL_CTX_set_alpn_protos(Result,PByte(PChar(Protos_ALPN2)),Length(Protos_ALPN2));
 end;

 SSL_CTX_set_options(Result,
                     SSL_OP_ALL or
                     SSL_OP_NO_COMPRESSION or
                     SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION or
                     SSL_MODE_RELEASE_BUFFERS);

 if SSL_CTX_load_store(Result) then
 begin
  //SSL_CTX_set_verify(Result,SSL_VERIFY_PEER,nil);
  SSL_CTX_set_verify_depth(Result,5);
 end;

end;

Procedure Websocket_eventcb(bev:Pbufferevent;events:SizeUInt;ctx:pointer); forward;

type
 TWebsocketData=class
   bev:Pbufferevent;
   ws_handshake:PfpWebsocket_handshake;
   ws_session:PfpWebsocket_session;
   time_kd:QWORD;
   url:RawByteString;
   procedure   Init(ssl_ctx:Pssl_ctx;hostname:PChar);
   procedure   Clear; virtual;
   function    connect_hostname(family:Integer;hostname:PAnsiChar;port:Word):Boolean;
   Destructor  Destroy; override;
   function    session_recv:integer;
   function    session_send:integer;
   function    ALPN:Boolean;
   procedure   NewSession;
   function    session_connect:integer;        virtual;
   function    session_reply:integer;          virtual;
   function    session_reconnect(sec:SizeUInt):Boolean;virtual;
   procedure   print_err;
 end;

 Pws_irc=^Tws_irc;

 Tws_irc=class(TWebsocketData)
  msg_send_buf:Pevbuffer;
  itself:Pws_irc;
  reply_pub:Boolean;
  recv_msg_chat:Boolean;
  reconnect:Boolean;
  msg_timer:Ptimer;
  not_slow:Boolean;
  login,oAuth,chat,
  room_id,display_name:RawByteString;
  color:DWORD;
  msg_buf:RawByteString;
  function    session_reply:integer;          override;
  function    session_reconnect(sec:SizeUInt):Boolean;override;
  procedure   Clear;   override;
  Destructor  Destroy; override;
 end;

 Tws_irc2=class(Tws_irc)
  function session_reconnect(sec:SizeUInt):Boolean; override;
 end;

 Tws_pub=class(TWebsocketData)
  reconnect:Boolean;
  ping_timer:Ptimer;
  pong_timer:Ptimer;
  oAuth,chat_id:RawByteString;
  nonce:RawByteString;
  msg_buf:TMemoryStream;
  function    session_reply:integer;   override;
  function    session_reconnect(sec:SizeUInt):Boolean;override;
  procedure   Clear;   override;
  Destructor  Destroy; override;
 end;

var
 ws_irc:Tws_irc;
 ws_pub:Tws_pub;
 ws_irc2:Tws_irc;

 ws_irc_rt:Ptimer;
 ws_pub_rt:Ptimer;
 ws_irc2_rt:Ptimer;

function create_ssl(ssl_ctx:Pssl_ctx):PSSL;
begin
 Result:=SSL_new(ssl_ctx);
 if not Assigned(Result) then
 begin
  Log(irc_log,1,['Could not create SSL/TLS session object: ',ERR_error_string(ERR_get_error(),nil)]);
 end;
end;

Const
 X509_CHECK_FLAG_NO_PARTIAL_WILDCARDS=$4;

procedure TWebsocketData.Init(ssl_ctx:Pssl_ctx;hostname:PChar);
Var
 FSSL:PSSL;
begin
 Clear;

 if Assigned(ssl_ctx) then
 begin
  FSSL:=create_ssl(ssl_ctx);
  SSL_set_hostflags(FSSL, X509_CHECK_FLAG_NO_PARTIAL_WILDCARDS);
  SSL_set1_host(FSSL,PByte(hostname));
  Log(irc_log,0,'Init SSL');
 end else
 begin
  FSSL:=nil;
  Log(irc_log,0,'Init');
 end;

 if Assigned(ssl_ctx) then
 begin
  bev:=bufferevent_openssl_socket_new(
     @pool, -1, FSSL, BUFFEREVENT_SSL_CONNECTING);
 end else
 begin
  bev:=bufferevent_socket_new(@pool,-1);
 end;

 bufferevent_setcb(bev,@Websocket_eventcb,Pointer(Self));
end;

function TWebsocketData.connect_hostname(family:Integer;hostname:PAnsiChar;port:Word):Boolean;
begin
 Log(irc_log,0,['connect_hostname:',hostname,':',port]);
 Result:=bufferevent_socket_connect_hostname(bev,family,hostname,port);
 if not Result then
 begin
  Log(irc_log,1,['error:bufferevent_socket_connect_hostname']);
 end;
end;

procedure TWebsocketData.Clear;
begin
 time_kd:=1800000;
 url:='';
 fpWebsocket_handshake_del(ws_handshake);
 fpWebsocket_session_del(ws_session);
 bufferevent_free(bev);
 ws_handshake:=nil;
 ws_session:=nil;
 bev:=nil;
end;

Destructor TWebsocketData.Destroy;
begin
 Clear;
 bufferevent_free(bev);
 inherited;
end;

function TWebsocketData.session_recv:integer;
var
 input:Pevbuffer;
 datapos,err:sizeUInt;
 readlen:sizeInt;
 vec:Piovec;

begin
 Result:=0;

 input:=bufferevent_get_input(bev);
 datapos:=evbuffer_get_length(input);

 if datapos=0 then Exit;

 While (datapos<>0) do
 begin

  vec:=evbuffer_peek(input);
  if not Assigned(vec) then Break;

  if (iovec_getlen(vec)>=datapos) then
  begin
   datapos:=0;
  end else
  begin
   datapos:=datapos-iovec_getlen(vec);
  end;

  readlen:=0;
  if (ws_session<>nil) then
  begin
   readlen:=fpWebsocket_session_mem_recv(ws_session,iovec_getdata(vec),iovec_getlen(vec));
  end else
  if (ws_handshake<>nil) then
  begin
   readlen:=fpWebsocket_handshake_mem_recv(ws_handshake,iovec_getdata(vec),iovec_getlen(vec));
   if fpWebsocket_handshake_complite(ws_handshake) then
   begin
    session_reply;
   end;
  end;

  evbuffer_drain(input,readlen);
  if readlen<0 then Break;
 end;

 if (ws_session<>nil) then
 begin
  err:=fpWebsocket_session_get_recv_err(ws_session);
  if err<>WS_ERR_OPENED then
  begin
   Result:=-1;
   Log(irc_log,1,['ws_recv_err:',err,' ',readlen]);
  end;
 end else
 begin
  if fpWebsocket_handshake_get_state(ws_handshake)=-1 then
  begin
   Result:=-1;
   Log(irc_log,1,['session_recv:',readlen]);
  end;
 end;

end;

Const
 OUTPUT_WOULDBLOCK_THRESHOLD=16*1024;

function TWebsocketData.session_send:integer;
Var
 output:Pevbuffer;
 i:sizeUInt;
 wsio_block:Tio_block;
begin
 Result:=0;

 output:=bufferevent_get_output(bev);

 repeat
  wsio_block:=Default(Tio_block);

  if (ws_session<>nil) then
  begin
   Result:=fpWebsocket_session_mem_send(ws_session,@wsio_block);
   if Result>0 then
   begin
    if (wsio_block.free=nil) and
       (wsio_block.user<>nil) then
    begin
     evbuffer_push(output,wsio_block.user);
    end else
    begin
     evbuffer_add_ref(output,
                      wsio_block.data,
                      0,
                      wsio_block.len,
                      wsio_block.free);
    end;
   end;
  end else
  if (ws_handshake<>nil) then
  begin
   Result:=fpWebsocket_handshake_mem_send(ws_handshake,@wsio_block);

   if Result>0 then
   begin
    evbuffer_add_ref(output,
                     wsio_block.data,
                     0,
                     wsio_block.len,
                     wsio_block.free);
   end;

   if fpWebsocket_handshake_complite(ws_handshake) then
   begin
    session_reply;
   end;
  end;

  if (evbuffer_get_length(output)>=OUTPUT_WOULDBLOCK_THRESHOLD) then
  begin
   Result:=0;
  end;

 until (Result<=0);

 bufferevent_write(bev);

 if (ws_session<>nil) then
 begin
  i:=fpWebsocket_session_get_send_err(ws_session);
  if i<>WS_ERR_OPENED then
  begin
   Result:=-1;
   Log(irc_log,1,['ws_send_err:',i]);
  end;
 end else
 begin
  if fpWebsocket_handshake_get_state(ws_handshake)=-1 then
  begin
   Result:=-1;
   Log(irc_log,1,['session_send:',Result]);
  end;
 end;

end;

function TWebsocketData.ALPN:Boolean;
Var
 FSSL:PSSL;
 _alpn:Pointer;
 _alpnlen:size_t;
 i:Integer;
begin
 Result:=True;
 FSSL:=bufferevent_openssl_get_ssl(bev);
 if Assigned(FSSL) then
 begin
  _alpn:=nil;
  _alpnlen:=0;
  SSL_get0_next_proto_negotiated(FSSL,@_alpn,@_alpnlen);
  if (_alpn=nil) then
  begin
   SSL_get0_alpn_selected(FSSL,@_alpn,@_alpnlen);
  end;
  if (_alpn<>nil) then
  begin
   Log(irc_log,0,['ALPN Select:',GetStr(_alpn,_alpnlen)]);
  end else
  begin
   Log(irc_log,1,'ALPN not Select');
   Exit(False);
  end;
  Log(irc_log,0,['SSL_version:',PChar(SSL_get_version(FSSL)),' SSL_cipher:',PChar(SSL_get_cipher_name(FSSL))]);
  i:=SSL_get_verify_result(FSSL);
  if (i<>0) then
  begin
   Log(irc_log,i,'Error [SSL_get_verify_result]');
   Exit(False);
  end;
  if (SSL_get0_peername(FSSL)=nil) then
  begin
   Log(irc_log,i,'Error [SSL_get0_peername]');
   Exit(False);
  end;
 end;
end;

procedure TWebsocketData.NewSession;
const
 origin='https://www.twitch.tv';
begin
 fpWebsocket_handshake_client_new(ws_handshake);
 fpWebsocket_handshake_set_secure(ws_handshake,bufferevent_openssl_get_ssl(bev)<>nil);
 fpWebsocket_handshake_select_version(ws_handshake,0);

 fpWebsocket_handshake_set_opt(ws_handshake,WS_OPT_URL,PAnsiChar(url)   ,Length(url));
 fpWebsocket_handshake_set_opt(ws_handshake,WS_OPT_ORG,PAnsiChar(origin),Length(origin));
end;

function TWebsocketData.session_connect:integer;
Var
 val:integer;
 fs:THandle;
begin
 Result:=0;
 Fs:=bufferevent_get_fd(bev);
 Log(irc_log,0,['bufferevent_get_fd:',Fs]);
 val:=1;//on
 fpsetsockopt(Fs,IPPROTO_TCP,TCP_NODELAY,@val,sizeof(val));
 SetKeepAlive(Fs,true,30,1,1);
 if not ALPN then Exit(-1);
 NewSession;
 session_send;
end;

function TWebsocketData.session_reply:integer;
begin
 Result:=0;
 Log(irc_log,0,'session_reply');
end;

function TWebsocketData.session_reconnect(sec:SizeUInt):Boolean;
begin
end;

procedure TWebsocketData.print_err;
Var
 FSSL:PSSL;
begin
 FSSL:=bufferevent_openssl_get_ssl(bev);
 if Assigned(FSSL) then
 begin
  Log(irc_log,SSL_get_error(FSSL,0),'[SSL_get_error]');
 end;
end;

Procedure Websocket_eventcb(bev:Pbufferevent;events:SizeUInt;ctx:pointer);
Var
 SessionData:TWebsocketData;
begin
 SessionData:=TWebsocketData(ctx);

 if (events and BEV_EVENT_ERROR)=0 then
 begin
  if (events and BEV_EVENT_READING)<>0 then
  begin
   if SessionData.session_recv<0 then
   begin
    SessionData.print_err;
    if not SessionData.session_reconnect(2) then
    begin
     SessionData.Free;
     Exit;
    end;
   end else
    bufferevent_read(bev);
  end;
  if ((events and BEV_EVENT_WRITING)<>0) or
     fpWebsocket_session_want_write(SessionData.ws_session) then
  begin
   if SessionData.session_send<0 then
   begin
    SessionData.print_err;
    if not SessionData.session_reconnect(2) then
    begin
     SessionData.Free;
     Exit;
    end;
   end;
  end;

  if (events and BEV_EVENT_CONNECTED)<>0 then
  begin
   Log(irc_log,0,['BEV_EVENT_CONNECTED:',bufferevent_get_fd(bev),':',GetThreadID]);
   if SessionData.session_connect<0 then
   begin
    SessionData.print_err;
    if not SessionData.session_reconnect(10) then
    begin
     SessionData.Free;
     Exit;
    end;
   end;
  end;

 end;

 if (events and (BEV_EVENT_EOF or BEV_EVENT_ERROR or BEV_EVENT_TIMEOUT))<>0 then
 begin
  Log(irc_log,0,['BEV_EVENT_ERROR_WS:',events and BEV_EVENT_EOF<>0]);
  if Assigned(SessionData) then
  begin
   SessionData.print_err;
   if not SessionData.session_reconnect(2) then
   begin
    SessionData.Free;
    Exit;
   end;
  end;
 end;

end;

procedure replyConnect_irc(const login,oAuth,chat:RawByteString;rep:Boolean);
Const
 Path='wss://irc-ws-r.chat.twitch.tv/';
Var
 URI:TURI;
 ctx:PSSL_CTX;
 port:Word;
begin
 URI:=parse_uri(Path);
 ctx:=nil;
 port:=URI.GetPort;
 Case LowerCase(URI.getProtocol()) of
  '',
   'ws':begin
         if port=0 then port:=80;
        end;
  'wss':begin
         if port=0 then port:=443;
         if wss_ctx=nil then wss_ctx:=create_ssl_ctx(false);
         ctx:=wss_ctx;
        end;
 end;
 Log(irc_log,0,['CONNECT TO:',URI.GetHost+':'+URI.GetPath,':',port]);

 if ws_irc=nil then
 begin
  ws_irc:=Tws_irc.Create;
  ws_irc.itself:=@ws_irc;
  ws_irc.recv_msg_chat:=True;
  ws_irc.reply_pub:=True;
 end;
 ws_irc.Init(ctx,PAnsiChar(URI.GetHost));

 ws_irc.url  :=Path;
 ws_irc.login:=login;
 ws_irc.oAuth:=oAuth;
 ws_irc.chat :=chat;

 if not ws_irc.connect_hostname(AF_INET,PAnsiChar(URI.GetHost),port) then
 begin
  if rep then
  begin
   ws_irc.reconnect:=true;
   if not ws_irc.session_reconnect(10) then FreeAndNil(ws_irc);
  end;
 end;

end;

procedure replyConnect_irc2(const login,oAuth,chat:RawByteString;rep:Boolean);
Const
 Path='wss://irc-ws-r.chat.twitch.tv/';
Var
 URI:TURI;
 ctx:PSSL_CTX;
 port:Word;
begin
 URI:=parse_uri(Path);
 ctx:=nil;
 port:=URI.GetPort;
 Case LowerCase(URI.getProtocol()) of
  '',
   'ws':begin
         if port=0 then port:=80;
        end;
  'wss':begin
         if port=0 then port:=443;
         if wss_ctx=nil then wss_ctx:=create_ssl_ctx(false);
         ctx:=wss_ctx;
        end;
 end;
 Log(irc_log,0,['CONNECT TO:',URI.GetHost+':'+URI.GetPath,':',port]);

 if ws_irc2=nil then
 begin
  ws_irc2:=Tws_irc.Create;
  ws_irc2.itself:=@ws_irc2;
  ws_irc2.recv_msg_chat:=False;
  ws_irc2.reply_pub:=False;
 end;
 ws_irc2.Init(ctx,PAnsiChar(URI.GetHost));

 ws_irc2.url  :=Path;
 ws_irc2.login:=login;
 ws_irc2.oAuth:=oAuth;
 ws_irc2.chat :=chat;

 if not ws_irc2.connect_hostname(AF_INET,PAnsiChar(URI.GetHost),port) then
 begin
  if rep then
  begin
   ws_irc2.reconnect:=true;
   if not ws_irc2.session_reconnect(10) then FreeAndNil(ws_irc2);
  end;
 end;

end;

type
 Pirc_Connect=^Tirc_Connect;
 Tirc_Connect=record
  login,oAuth,chat:PAnsiChar;
 end;

function CopyPchar(Src:PAnsiChar;Len:size_t):PAnsiChar;
begin
 Result:=nil;
 if (Src=nil) or (Len=0) then Exit;
 Result:=GetMem(Len+1);
 Move(Src^,Result^,Len);
 Result[Len]:=#0;
end;

procedure Free_pirc_Connect(P:Pirc_Connect);
begin
 if P<>nil then
 begin
  FreeMem(P^.login  );
  FreeMem(P^.oAuth  );
  FreeMem(P^.chat   );
  FreeMem(P);
 end;
end;

Procedure _ws_irc_rt_cb(ev:Ptimer;arg:pointer);
var
 ws_irc:Tws_irc;
begin
 ws_irc:=Tws_irc(arg);
 if ws_irc<>nil then
 begin
  replyConnect_irc(ws_irc.login,ws_irc.oAuth,ws_irc.chat,true);
 end;
 evtimer_del(ws_irc_rt);
 ws_irc_rt:=nil;
end;

function Tws_irc.session_reconnect(sec:SizeUInt):Boolean;
begin
 Result:=False;
 if reconnect then
 begin
  evtimer_reuse(ws_irc_rt,@pool,@_ws_irc_rt_cb,Pointer(Self));
  evtimer_add  (ws_irc_rt,sec*1000000);
  Result:=True;
 end else
 begin
  Main.push_login(False);
 end;
end;

procedure Tws_irc.Clear;
begin
 inherited;
 login:='';
 oAuth:='';
 chat:='';
 msg_buf:='';
 evtimer_del(msg_timer);
 msg_timer:=nil;
end;

Destructor Tws_irc.Destroy;
begin
 if (itself<>nil) and (itself^=self) then itself^:=nil;
 inherited;
end;

Procedure _ws_irc2_rt_cb(ev:Ptimer;arg:pointer);
begin
 ws_irc:=Tws_irc(arg);
 if ws_irc<>nil then
 begin
  replyConnect_irc2(ws_irc.login,ws_irc.oAuth,ws_irc.chat,true);
 end;
 evtimer_del(ws_irc2_rt);
 ws_irc2_rt:=nil;
end;

function Tws_irc2.session_reconnect(sec:SizeUInt):Boolean;
begin
 Result:=False;
 if reconnect then
 begin
  evtimer_reuse(ws_irc2_rt,@pool,@_ws_irc2_rt_cb,Pointer(Self));
  evtimer_add  (ws_irc2_rt,sec*1000000);
  Result:=True;
 end;
end;

Const
 PRIVMSG_HEAD='PRIVMSG #';
 PRIVMSG_TAIL=' :';
 PRIVMSG_LEN=Length(PRIVMSG_HEAD)+Length(PRIVMSG_TAIL);

function _submit_vec(session:PfpWebsocket_session;
                     source:PfpWebsocket_data_provider;
                     frame_len:size_t;
                     block:Pio_block):ssize_t;

 procedure _send_data_vec(data:Pointer;datalen:size_t);
 Var
  Node:Piovec;
 begin
  Node:=GetMem(datalen+SizeOf(Tiovec));
  if Node=nil then Exit;
  With Node^ do
  begin
   base:=@PByte(Node)[SizeOf(Tiovec)];
   len:=datalen;
   pos:=0;
   buf_free:=nil;
   vec_free:=Freemem_ptr;
  end;
  Move(data^,Node^.base^,datalen);
  block^.data:=iovec_getdata(Node);
  block^.len :=iovec_getlen(Node);
  block^.free:=nil;
  block^.user:=Node;
 end;

var
 ws_irc:Tws_irc;
begin

 ws_irc:=Tws_irc(fpWebsocket_session_get_user_data(session));
 if ws_irc=nil then Exit(WS_CB_ERR);

 Case PtrUint(source^.data) of
  0:begin
     Result:=WS_CB_CON;
     _send_data_vec(PAnsiChar(PRIVMSG_HEAD),Length(PRIVMSG_HEAD));
     PtrUint(source^.data):=1;
    end;
  1:begin
     Result:=WS_CB_CON;
     _send_data_vec(PAnsiChar(ws_irc.chat),Length(ws_irc.chat));
     PtrUint(source^.data):=2;
    end;
  2:begin
     Result:=WS_CB_CON;
     _send_data_vec(PAnsiChar(PRIVMSG_TAIL),Length(PRIVMSG_TAIL));
     PtrUint(source^.data):=3;
    end;
  3:begin
     Result:=WS_CB_FIN;
     block^.data:=iovec_getdata(source^.user);
     block^.len :=iovec_getlen(source^.user);
     block^.free:=nil;
     block^.user:=source^.user;
     source^.data:=nil;
     PtrUint(source^.data):=4;
    end;
  else
    Result:=WS_CB_ERR;
 end;

 //Result:=WS_CB_FIN;
 //block^.data:=iovec_getdata(source^.user);//source^.data;
 //block^.len :=frame_len;
 //block^.free:=nil;
 //block^.user:=source^.user;
 //source^.data:=nil;
 //source^.user:=nil;
end;

function fpWebsocket_session_submit_msg_vec(session:PfpWebsocket_session;msg:Piovec;chat_len:ssize_t):Boolean;
Const
 op_code=$01;
var
 data_prd:TfpWebsocket_data_provider;
begin
 Result:=false;
 if (session=nil) then Exit;

 if (msg=nil) or (msg^.len=0) then
 begin
  Result:=fpWebsocket_session_submit_frame_stream(session,nil,0,op_code);
 end else
 begin
  data_prd.data    :=nil; //iovec_getdata(v);
  data_prd.user    :=msg;
  data_prd.close_cb:=nil;
  data_prd.read_cb :=@_submit_vec;
  Result:=fpWebsocket_session_submit_frame_stream(session,@data_prd,iovec_getlen(msg)+chat_len+PRIVMSG_LEN,op_code);
 end;

 if not Result then
 begin
  iovec_free(msg);
 end;
end;

function Utf8FixCut(P:PByte;Len:SizeInt):SizeInt; inline;
begin
 Inc(P,Len-1);
 While (Len>0) and (P^>127) do
 begin
  Dec(P);
  Dec(Len);
 end;
 Result:=Len;
end;

Procedure _submit_tm(ev:Ptimer;arg:pointer);
Const
 max_msg_size=500;
var
 ws_irc:Tws_irc;
 msg_send_buf:Pevbuffer;

 v:Piovec;
 Len:SizeInt;

 PC:TPrivMsgCfg;
begin
 ws_irc:=Tws_irc(arg);
 msg_send_buf:=ws_irc.msg_send_buf;
 v:=evbuffer_peek(msg_send_buf);
 if (v<>nil) then
 begin
  if iovec_getlen(v)>max_msg_size then
  begin
   Len:=Utf8FixCut(iovec_getdata(v),max_msg_size);
   v:=GetMem(Len+SizeOf(Tiovec));
   With v^ do
   begin
    base:=@PByte(v)[SizeOf(Tiovec)];
    len:=Len;
    pos:=0;
    buf_free:=nil;
    vec_free:=Freemem_ptr;
   end;
   evbuffer_remove(msg_send_buf,v^.base,Len);
  end else
  begin
   v:=evbuffer_pop(msg_send_buf);
  end;

  Log(irc_log,0,['<',GetStr(iovec_getdata(v),iovec_getlen(v))]);

  if ws_irc.recv_msg_chat then
  begin
   PC:=Default(TPrivMsgCfg);
   PC.PS:=[pm_self];
   PC.Color:=ws_irc.color;
   main.push_chat(PC,ws_irc.login,ws_irc.display_name,GetStr(iovec_getdata(v),iovec_getlen(v)));
  end;

  //Writeln(GetStr(iovec_getdata(v),iovec_getlen(v)));
  fpWebsocket_session_submit_msg_vec(ws_irc.ws_session,v,Length(ws_irc.chat));

  if ws_irc.msg_timer=nil then
  begin
   ws_irc.msg_timer:=evtimer_new(@pool,@_submit_tm,arg);
  end;

  ws_irc.session_send;

  evtimer_add(ws_irc.msg_timer,ws_irc.time_kd);
 end else
 begin

  evtimer_del(ws_irc.msg_timer);
  ws_irc.msg_timer:=nil;
 end;

end;

function _gen_nonce:RawByteString; forward;

procedure _submit_msg(const msg:RawByteString;var msg_send_buf:Pevbuffer;var ws_irc:Tws_irc);
//var
// S:RawByteString;
begin
 if msg_send_buf=nil then
 begin
  msg_send_buf:=evbuffer_new;
 end;

 if (msg='') or (ws_irc=nil) then Exit;

 ws_irc.msg_send_buf:=msg_send_buf;

 //S:={'@client-nonce='+_gen_nonce+' '+}'PRIVMSG #'+ws_irc.chat+' :'+msg;

 evbuffer_add(msg_send_buf,PAnsiChar(msg),Length(msg));

 if ws_irc.msg_timer=nil then
 begin
  _submit_tm(ws_irc.msg_timer,ws_irc);
 end else
 begin
  evtimer_add(ws_irc.msg_timer,ws_irc.time_kd);
 end;
end;

Type
 Tmsg_parse=object
  cmd,
  chat,
  display_name,
  user,
  room_id,
  msg_id,
  msg:RawByteString;

  slow:QWORD;

  RS:TRoomStates;

  PC:TPrivMsgCfg;

  procedure parse(var S:RawByteString);
 end;

procedure parse_badges(param:RawByteString;var PC:TPrivMsgCfg);
var
 v,n:RawByteString;
 i:SizeInt;
begin
 repeat
  i:=System.IndexChar(PAnsiChar(param)^,Length(param),',');
  if i<>-1 then
  begin
   v:=Copy(param,1,i);
   param:=Copy(param,i+2,Length(param)-(i+1));
  end else
  begin
   v:=param;
   param:='';
  end;

  i:=System.IndexChar(PAnsiChar(v)^,Length(v),'/');
  if i<>-1 then
  begin
   n:=Copy(v,1,i);
   v:=Copy(v,i+2,Length(v)-(i+1));
  end else
  begin
   n:=v;
   v:='';
  end;

  //Writeln(n,'*',v,'*');

  case n of
   'admin'      :PC.PS:=PC.PS+[pm_admin      ];
   'broadcaster':PC.PS:=PC.PS+[pm_broadcaster];
   'global_mod' :PC.PS:=PC.PS+[pm_global_mod ];
   'moderator'  :PC.PS:=PC.PS+[pm_moderator  ];
   'vip'        :PC.PS:=PC.PS+[pm_vip        ];
   'subscriber' :PC.subscriber_s:=StrToDWordDef(v,0);
   'sub-gifter' :PC.sub_gifter  :=StrToDWordDef(v,0);
  end;

 until (param='');
end;

procedure parse_badges_info(param:RawByteString;var PC:TPrivMsgCfg);
var
 v,n:RawByteString;
 i:SizeInt;
begin
 repeat
  i:=System.IndexChar(PAnsiChar(param)^,Length(param),',');
  if i<>-1 then
  begin
   v:=Copy(param,1,i);
   param:=Copy(param,i+2,Length(param)-(i+1));
  end else
  begin
   v:=param;
   param:='';
  end;

  i:=System.IndexChar(PAnsiChar(v)^,Length(v),'/');
  if i<>-1 then
  begin
   n:=Copy(v,1,i);
   v:=Copy(v,i+2,Length(v)-(i+1));
  end else
  begin
   n:=v;
   v:='';
  end;

  //Writeln(n,'*',v,'*');

  case n of
   'subscriber':PC.subscriber_m:=StrToDWordDef(v,0);
  end;

 until (param='');
end;

Const
 default_colors:array[0..14] of DWORD=(
  $FF0000, //"Red"
  $0000FF, //"Blue"
  $00FF00, //"Green"
  $B22222, //"FireBrick"
  $FF7F50, //"Coral"
  $9ACD32, //"YellowGreen"
  $FF4500, //"OrangeRed"
  $2E8B57, //"SeaGreen"
  $DAA520, //"GoldenRod"
  $D2691E, //"Chocolate"
  $5F9EA0, //"CadetBlue"
  $1E90FF, //"DodgerBlue"
  $FF69B4, //"HotPink"
  $8A2BE2, //"BlueViolet"
  $00FF7F  //"SpringGreen"
 );

function get_color_for_user(const name:RawByteString):DWORD;
var
 n:Word;
begin
 Result:=0;
 if name='' then Exit;
 n:=ord(name[Low(name)])+ord(name[High(name)]);
 Result:=default_colors[n mod Length(default_colors)];
end;

function convert_color(S:RawByteString;const name:RawByteString):DWORD;
var
 b:Byte;
begin
 Result:=0;
 if S='' then
 begin
  Result:=get_color_for_user(name);
  Exit;
 end;
 S[1]:='$';
 Result:=StrToDWordDef(S,0);
 b:=PByte(@Result)[0];
 PByte(@Result)[0]:=PByte(@Result)[2];
 PByte(@Result)[2]:=b;
end;

function UIDToString(const GUID: TGUID): RawByteString;
const
 HexTbl:array[0..15] of AnsiChar='0123456789abcdef';
var
 P:PAnsiChar;

 procedure hexstr_d(val:DWORD); inline;
 var
  i:Byte;
 begin
  for i:=7 downto 0 do
  begin
   P[i]:=HexTbl[val and $f];
   val:=val shr 4;
  end;
  P:=P+8;
 end;

 procedure hexstr_w(val:WORD); inline;
 var
  i:Byte;
 begin
  for i:=3 downto 0 do
  begin
   P[i]:=HexTbl[val and $f];
   val:=val shr 4;
  end;
  P:=P+4;
 end;

 procedure hexstr_b(val:Byte); inline;
 begin
  P[1]:=HexTbl[val and $f];
  P[0]:=HexTbl[val shr 4];
  P:=P+2;
 end;

 procedure nextChar(c:AnsiChar); inline;
 begin
  P^:=C;
  Inc(P);
 end;

begin
 SetLength(Result, 36);
 P:=PAnsiChar(Result);

 hexstr_d(GUID.D1);
 nextChar('-');
 hexstr_w(GUID.D2);
 nextChar('-');
 hexstr_w(GUID.D3);
 nextChar('-');
 hexstr_b(GUID.D4[0]);
 hexstr_b(GUID.D4[1]);
 nextChar('-');
 hexstr_b(GUID.D4[2]);
 hexstr_b(GUID.D4[3]);
 hexstr_b(GUID.D4[4]);
 hexstr_b(GUID.D4[5]);
 hexstr_b(GUID.D4[6]);
 hexstr_b(GUID.D4[7]);
end;

function TryStringToUID(const S: RawByteString; out Guid: TGUID): Boolean;
var
  e: Boolean;
  p: PChar;

  function rb: Byte;
  begin
   case p^ of
     '0'..'9': Result := Byte(p^) - Byte('0');
     'a'..'f': Result := Byte(p^) - Byte('a') + 10;
     'A'..'F': Result := Byte(p^) - Byte('A') + 10;
     else
     begin
       e := False;
       Result:=0;
     end;
   end;
   Inc(p);
  end;

  procedure nextChar(c: Char); inline;
  begin
    if p^ <> c then e := False;
    Inc(p);
  end;

begin
  if Length(S)<>36 then Exit(False);
  e := True;
  p := PChar(S);
  Guid.D1 := rb shl 28 or rb shl 24 or rb shl 20 or rb shl 16 or rb shl 12 or rb shl 8 or rb shl 4 or rb;
  nextChar('-');
  Guid.D2 := rb shl 12 or rb shl 8 or rb shl 4 or rb;
  nextChar('-');
  Guid.D3 := rb shl 12 or rb shl 8 or rb shl 4 or rb;
  nextChar('-');
  Guid.D4[0] := rb shl 4 or rb;
  Guid.D4[1] := rb shl 4 or rb;
  nextChar('-');
  Guid.D4[2] := rb shl 4 or rb;
  Guid.D4[3] := rb shl 4 or rb;
  Guid.D4[4] := rb shl 4 or rb;
  Guid.D4[5] := rb shl 4 or rb;
  Guid.D4[6] := rb shl 4 or rb;
  Guid.D4[7] := rb shl 4 or rb;
  Result := e;
end;

procedure Tmsg_parse.parse(var S:RawByteString);
var
 v,n,
 param:RawByteString;
 i:SizeInt;
begin
 param:='';

 i:=Pos(#13#10,S);
 if i=0 then
 begin
  msg:=S;
  S:='';
 end else
 begin
  msg:=Copy(S,1,(i-1));
  S:=Copy(S,i+2,Length(S)-(i-3));
 end;

 if msg[1]='@' then
 begin
  i:=System.IndexChar(PAnsiChar(msg)^,Length(msg),' ');
  if i<>-1 then
  begin
   param:=Copy(msg,2,i-1);
   msg:=Copy(msg,i+3,Length(msg)-(i+2));
  end else
  begin
   msg:='';
  end;
 end;

 i:=System.IndexChar(PAnsiChar(msg)^,Length(msg),' ');
 if i<>-1 then
 begin
  user:=Copy(msg,1,i);
  msg:=Copy(msg,i+2,Length(msg)-(i+1));

  i:=System.IndexChar(PAnsiChar(user)^,Length(user),'!');
  if i<>-1 then
  begin
   user:=Copy(user,i+2,Length(user)-(i+1));
  end;

  i:=System.IndexChar(PAnsiChar(user)^,Length(user),'@');
  if i<>-1 then
  begin
   user:=Copy(user,1,i);
   //:=Copy(user,i+2,Length(user)-(i+1));
  end;

 end;

 i:=System.IndexChar(PAnsiChar(msg)^,Length(msg),' ');
 if i<>-1 then
 begin
  cmd:=Copy(msg,1,i);
  msg:=Copy(msg,i+2,Length(msg)-(i+1));
 end;

 i:=System.IndexChar(PAnsiChar(msg)^,Length(msg),' ');
 if i<>-1 then
 begin
  chat:=Copy(msg,1,i);
  msg:=Copy(msg,i+3,Length(msg)-(i+2));
 end;

 //Writeln(cmd);

 repeat
  i:=System.IndexChar(PAnsiChar(param)^,Length(param),';');
  if i<>-1 then
  begin
   v:=Copy(param,1,i);
   param:=Copy(param,i+2,Length(param)-(i+1));
  end else
  begin
   v:=param;
   param:='';
  end;

  i:=System.IndexChar(PAnsiChar(v)^,Length(v),'=');
  if i<>-1 then
  begin
   n:=Copy(v,1,i);
   v:=Copy(v,i+2,Length(v)-(i+1));
  end else
  begin
   n:=v;
   v:='';
  end;

  //Writeln('u:',user,'*');

  //Writeln(n,'*',v,'*');

  Case n of
   'id':TryStringToUID(v,PC.uid);
   'msg-id'        :begin
                     msg_id:=v;
                     case msg_id of
                      'highlighted-message':PC.PS:=PC.PS+[pm_highlighted];
                     end;
                    end;

   'emote-only'    :if v<>'0' then RS:=RS+[Rs_emote_only];
   'followers-only':if v<>'0' then RS:=RS+[Rs_followers_only];
   'r9k'           :if v<>'0' then RS:=RS+[Rs_r9k];
   'rituals'       :if v<>'0' then RS:=RS+[Rs_rituals];
   'subs-only'     :if v<>'0' then RS:=RS+[Rs_subs_only];
   'slow'          :slow:=StrToQWordDef(v,0);

   'room-id'       :room_id:=v;

   'display-name'  :display_name:=v;

   'badge-info'    :parse_badges_info(v,PC);
   'badges'        :parse_badges(v,PC);

   'user-id'       :PC.user_id:=StrToQWordDef(v,0);

   'color'         :PC.Color:=convert_color(v,user);

  end;

  if sub_mod.room_tag=n then
  begin
   if v<>'0' then RS:=RS+[Rs_room_tag];
  end;

 until (param='');

end;

procedure replyConnect_pub(const oAuth,chat_id:RawByteString;rep:Boolean); forward;

function _on_message_irc_cb(session:PfpWebsocket_session;data:Pointer;len:size_t;flags:size_t):ssize_t;
Const
 TW_PONG:RawByteString='PONG :tmi.twitch.tv'#13#10;
var
 ws_irc:Tws_irc;
 msg:RawByteString;
 msg_parse:Tmsg_parse;

begin
 Result:=0;
 if (flags and WS_FLAG_TXT)<>0 then
 begin
  ws_irc:=Tws_irc(fpWebsocket_session_get_user_data(session));
  ws_irc.msg_buf:=ws_irc.msg_buf+GetStr(data,len);
  if (flags and WS_FLAG_FIN)<>0 then
  begin
   msg:=ws_irc.msg_buf;

   case msg of
    'PING :tmi.twitch.tv'#13#10:
    begin
     fpWebsocket_session_submit_text(session,PAnsiChar(TW_PONG),Length(TW_PONG));
     ws_irc.msg_buf:='';
     Exit;
    end;
   end;

   if Length(msg)=0 then Exit;

   //Writeln(msg);

   repeat
    msg_parse:=Default(Tmsg_parse);
    msg_parse.parse(msg);

    case msg_parse.cmd of
     'USERSTATE':begin
                  ws_irc.color       :=msg_parse.PC.Color;
                  ws_irc.display_name:=msg_parse.display_name;
                  ws_irc.not_slow:=(msg_parse.PC.PS*[pm_admin,
                                                     pm_broadcaster,
                                                     pm_global_mod,
                                                     pm_moderator]<>[]) or
                                   (msg_parse.PC.subscriber_m<>0);
                 end;

     'ROOMSTATE':begin
                  //room-id*54742538*
                  if (not ws_irc.not_slow) and (msg_parse.slow<>0) then
                  begin
                   ws_irc.time_kd:=(msg_parse.slow+1)*1000000;
                  end;

                  if ws_irc.room_id='' then
                  begin
                   ws_irc.room_id:=msg_parse.room_id;
                   if ws_irc.room_id='' then
                   begin
                    main.push_notice('room-id','Ошибка подключения к событиям!');
                   end else
                   if ws_irc.reply_pub then
                   begin
                    replyConnect_pub(ws_irc.oAuth,ws_irc.room_id,false);
                   end;
                  end;

                  if ws_irc.recv_msg_chat then
                   push_room_states(msg_parse.RS);
                 end;

     '001':if ws_irc.recv_msg_chat then
           begin
            main.push_notice('001','Добро пожаловать в чат!');
            main.push_login(True);
           end;
     'NOTICE':begin
               main.push_notice(msg_parse.msg_id,msg_parse.msg);
               case msg_parse.msg of
                'Login authentication failed':
                begin
                 Result:=-1;
                 ws_irc.reconnect:=false;
                end;
               end;
              end;
     'PRIVMSG':if ws_irc.recv_msg_chat then
               begin
                main.push_chat(msg_parse.PC,msg_parse.user,msg_parse.display_name,msg_parse.msg);
               end;

     'WHISPER':if ws_irc.recv_msg_chat then
               begin
                msg_parse.PC.PS:=msg_parse.PC.PS+[pm_whisper];
                main.push_chat(msg_parse.PC,msg_parse.user,msg_parse.display_name,msg_parse.msg);
               end;

    end;

    {Writeln(msg_parse.cmd,'*',
            msg_parse.chat,'*',
            msg_parse.display_name,'*',
            msg_parse.user,'*',
            msg_parse.msg,'*');}

   until (msg='');


   ws_irc.msg_buf:='';
  end;

 end;

end;

function Tws_irc.session_reply:integer;
Const
 r1:RawByteString='CAP REQ :twitch.tv/commands';
 r2:RawByteString='CAP REQ :twitch.tv/tags';

var
 S:RawByteString;
begin
 Result:=inherited;

 reconnect:=true;

 fpWebsocket_session_new(ws_session,ws_handshake);
 fpWebsocket_session_set_user_data(ws_session,Pointer(self));
 fpWebsocket_session_set_message_cb(ws_session,@_on_message_irc_cb);

 fpWebsocket_handshake_del(ws_handshake);
 ws_handshake:=nil;

 S:='PASS oauth:'+oAuth;
 fpWebsocket_session_submit_text(ws_session,PAnsiChar(S),Length(S));
 S:='NICK '+login;
 fpWebsocket_session_submit_text(ws_session,PAnsiChar(S),Length(S));

 fpWebsocket_session_submit_text(ws_session,PAnsiChar(r1),Length(r1));
 fpWebsocket_session_submit_text(ws_session,PAnsiChar(r2),Length(r2));

 S:='USER '+login+' 8 * :'+login;
 fpWebsocket_session_submit_text(ws_session,PAnsiChar(S),Length(S));
 S:='JOIN #'+chat;
 fpWebsocket_session_submit_text(ws_session,PAnsiChar(S),Length(S));
end;

procedure replyConnect_pub(const oAuth,chat_id:RawByteString;rep:Boolean);
Const
 Path='wss://pubsub-edge.twitch.tv/v1';
Var
 URI:TURI;
 ctx:PSSL_CTX;
 port:Word;
begin
 URI:=parse_uri(Path);
 ctx:=nil;
 port:=URI.GetPort;
 Case LowerCase(URI.getProtocol()) of
  '',
   'ws':begin
         if port=0 then port:=80;
        end;
  'wss':begin
         if port=0 then port:=443;
         if wss_ctx=nil then wss_ctx:=create_ssl_ctx(false);
         ctx:=wss_ctx;
        end;
 end;
 Log(irc_log,0,['CONNECT TO:',URI.GetHost+':'+URI.GetPath,':',port]);

 if ws_pub=nil then
 begin
  ws_pub:=Tws_pub.Create;
 end;
 ws_pub.Init(ctx,PAnsiChar(URI.GetHost));

 ws_pub.url    :=Path;
 ws_pub.oAuth  :=oAuth;
 ws_pub.chat_id:=chat_id;

 if not ws_pub.connect_hostname(AF_INET,PAnsiChar(URI.GetHost),port) then
 begin
  if rep then
  begin
   ws_pub.reconnect:=true;
   if not ws_pub.session_reconnect(10) then FreeAndNil(ws_pub);
  end;
 end;
end;

Procedure _ws_pub_rt_cb(ev:Ptimer;arg:pointer);
begin
 if ws_pub<>nil then
 begin
  replyConnect_pub(ws_pub.oAuth,ws_pub.chat_id,true);
 end;
 evtimer_del(ws_pub_rt);
 ws_pub_rt:=nil;
end;

function Tws_pub.session_reconnect(sec:SizeUInt):Boolean;
begin
 Result:=False;
 if reconnect then
 begin
  Result:=True;
  evtimer_reuse(ws_pub_rt,@pool,@_ws_pub_rt_cb,nil);
  evtimer_add  (ws_pub_rt,sec*1000000);
 end;
end;

Procedure _irc_Connect_post(param1:SizeUInt;param2:Pointer);
begin
 case param1 of
  0:begin
     FreeAndNil(ws_irc);
     FreeAndNil(ws_pub);
     With Pirc_Connect(param2)^ do
     begin
      replyConnect_irc(GetStr(login,StrLen(login)),GetStr(oAuth,StrLen(oAuth)),GetStr(chat,StrLen(chat)),false);
     end;
     Free_pirc_Connect(Pirc_Connect(param2));
    end;
  1:begin
     evbuffer_clear(msg_send_buf);
     evbuffer_clear(msg_send_buf2);
     FreeAndNil(ws_irc);
     FreeAndNil(ws_pub);
     FreeAndNil(ws_irc2);
    end;
  2:begin
     if ws_irc<>nil then
     begin
      _submit_msg(GetStr(param2,StrLen(param2)),msg_send_buf,ws_irc);
     end;
     FreeMem(param2);
    end;
  3:begin
     if Assigned(ws_pub) then
     begin
      ws_pub.reconnect:=true;
      bufferevent_openssl_shutdown(ws_pub.bev);
     end;
    end;
  4:begin
     FreeAndNil(ws_irc2);
     With Pirc_Connect(param2)^ do
     begin
      replyConnect_irc2(GetStr(login,StrLen(login)),GetStr(oAuth,StrLen(oAuth)),GetStr(chat,StrLen(chat)),false);
     end;
     Free_pirc_Connect(Pirc_Connect(param2));
    end;
  5:begin
     if ws_irc2<>nil then
     begin
      _submit_msg(GetStr(param2,StrLen(param2)),msg_send_buf2,ws_irc2);
     end;
     FreeMem(param2);
    end;
 end;
end;

procedure reply_irc_Connect(const login,oAuth,chat:RawByteString);
var
 P:Pirc_Connect;
begin
 P:=GetMem(SizeOf(Tirc_Connect));
 P^.login  :=CopyPchar(PAnsiChar(login)  ,Length(login));
 P^.oAuth  :=CopyPchar(PAnsiChar(oAuth)  ,Length(oAuth));
 P^.chat   :=CopyPchar(PAnsiChar(chat)   ,Length(chat));
 evpool_post(@pool,@_irc_Connect_post,0,P);
end;

procedure reply_irc_Disconnect;
begin
 evpool_post(@pool,@_irc_Connect_post,1,nil);
end;

procedure reply_irc_msg(const msg:RawByteString);
begin
 evpool_post(@pool,@_irc_Connect_post,2,CopyPchar(PAnsiChar(msg),Length(msg)));
end;

procedure reply_irc_reconnect;
begin
 evpool_post(@pool,@_irc_Connect_post,3,nil);
end;

procedure reply_irc_Connect2(const login,oAuth,chat:RawByteString);
var
 P:Pirc_Connect;
begin
 P:=GetMem(SizeOf(Tirc_Connect));
 P^.login  :=CopyPchar(PAnsiChar(login)  ,Length(login));
 P^.oAuth  :=CopyPchar(PAnsiChar(oAuth)  ,Length(oAuth));
 P^.chat   :=CopyPchar(PAnsiChar(chat)   ,Length(chat));
 evpool_post(@pool,@_irc_Connect_post,4,P);
end;

procedure reply_irc_msg2(const msg:RawByteString);
begin
 evpool_post(@pool,@_irc_Connect_post,5,CopyPchar(PAnsiChar(msg),Length(msg)));
end;

Const
 TW_PING='{ "type": "PING" }';
 TW_PONG='{ "type": "PONG" }';

function _on_message_pub_cb(session:PfpWebsocket_session;data:Pointer;len:size_t;flags:size_t):ssize_t;
var
 ws_pub:Tws_pub;
 msg:RawByteString;
 msg1{,msg2}:TJson;

begin
 Result:=0;

 if (flags and WS_FLAG_TXT)<>0 then
 begin
  ws_pub:=Tws_pub(fpWebsocket_session_get_user_data(session));

  if ws_pub.msg_buf=nil then
  begin
   ws_pub.msg_buf:=TMemoryStream.Create;
  end;
  ws_pub.msg_buf.Write(data^,len);

  if (flags and WS_FLAG_FIN)<>0 then
  begin

   msg:=GetStr(ws_pub.msg_buf.Memory,ws_pub.msg_buf.Size);
   //Writeln('message:',msg,'*');
   Log(irc_log,1,['message:',msg]);

   ws_pub.msg_buf.Position:=0;
   msg1:=Default(TJson);
   try
    msg1:=TJson.New(ws_pub.msg_buf);
   except
   end;

   Case msg1.Path['type'].AsStr of
    'RECONNECT':begin
                 Log(irc_log,1,['RECONNECT']);
                 ws_pub.reconnect:=true;
                 bufferevent_openssl_shutdown(ws_pub.bev);
                end;
    'PING':begin
            fpWebsocket_session_submit_text(session,PAnsiChar(TW_PONG),Length(TW_PONG));
           end;
    'PONG':begin
            evtimer_del(ws_pub.pong_timer);
            ws_pub.pong_timer:=nil;
           end;
    'RESPONSE':if ws_pub.nonce=msg1.Path['nonce'].AsStr then
               begin
                msg:=msg1.Path['error'].AsStr;
                if msg<>'' then Result:=-1;
                if msg='' then msg:='Ожидаем события поинтов!';
                main.push_notice('pub',msg);

                //main.push_reward('{"type":"custom-reward-updated","data":{"timestamp":"2020-07-11T12:47:47.04638314Z","updated_reward":{"id":"6a39b2f0-af31-4c40-a130-3ea08e9ec79a","channel_id":"54742538","title":"Anti Emote Mode","prompt":"ОФАЕМ 15 минут emote-МОда","cost":30001,"is_user_input_required":true,"is_sub_only":false,"image":null,"default_image":{"url_1x":"https://static-cdn.jtvnw.net/custom-reward-images/default-1.png","url_2x":"https://static-cdn.jtvnw.net/custom-reward-images/default-2.png","url_4x":"https://static-cdn.jtvnw.net/custom-reward-images/default-4.png"},"background_color":"#8205B3","is_enabled":false,"is_paused":false,"is_in_stock":false,"max_per_stream":{"is_enabled":true,"max_per_stream":10},"should_redemptions_skip_request_queue":false,"template_id":null,"updated_for_indicator_at":"2019-12-21T20:50:39.00014802Z"}}}');

               end;
    'MESSAGE':begin

               msg:=msg1.Path['data.message'].AsStr;

               main.push_reward(msg);
               Log(irc_log,1,['push_reward:',msg]);

               {ws_pub.msg_buf.Clear;
               ws_pub.msg_buf.Write(PAnsiChar(msg)^,Length(msg));
               ws_pub.msg_buf.Position:=0;

               msg2:=Default(TJson);
               try
                msg2:=TJson.New(ws_pub.msg_buf);
               except
               end;

               if msg2.Path['type'].AsStr='reward-redeemed' then
               begin
                Writeln('id               :',msg2.Path['data.redemption.id'].AsStr);
                Writeln('user.id          :',msg2.Path['data.redemption.user.id'].AsStr);
                Writeln('user.login       :',msg2.Path['data.redemption.user.login'].AsStr);
                Writeln('user.display_name:',msg2.Path['data.redemption.user.display_name'].AsStr);
                Writeln('redeemed_at      :',msg2.Path['data.redemption.redeemed_at'].AsStr);
                Writeln('reward.id        :',msg2.Path['data.redemption.reward.id'].AsStr);
                Writeln('reward.title     :',msg2.Path['data.redemption.reward.title'].AsStr);
                Writeln('reward.prompt    :',msg2.Path['data.redemption.reward.prompt'].AsStr);
                Writeln('user_input       :',msg2.Path['data.redemption.user_input'].AsStr);
                Writeln('status           :',msg2.Path['data.redemption.status'].AsStr);
               end;

               msg2.Free;
               }

              end;
   end;

   msg1.Free;

   ws_pub.msg_buf.Clear;

  end;

 end;


end;

const
 _ping_kd=4*60*1000000;
 _ping_jt=30*1000000;
 _pong_kd=10*1000000;

function ping_kd:Int64;
var
 Context:TMTRandomContext;
begin
 Context:=Default(TMTRandomContext);
 RandomInit(Context);
 Result:=_ping_kd+Random(Context,_ping_jt);
end;

Procedure _pong_pub(ev:Ptimer;arg:pointer);
var
 ws_pub:Tws_pub;
begin
 Pointer(ws_pub):=arg;
 if Assigned(ws_pub) then
 begin
  ws_pub.reconnect:=true;
  bufferevent_openssl_shutdown(ws_pub.bev);
 end;
end;

Procedure _ping_pub(ev:Ptimer;arg:pointer);
var
 ws_session:PfpWebsocket_session;
 ws_pub:Tws_pub;
begin
 Pointer(ws_pub):=arg;
 ws_session:=ws_pub.ws_session;

 fpWebsocket_session_submit_text(ws_session,PAnsiChar(TW_PING),Length(TW_PING));
 evtimer_add(ev,ping_kd);

 ws_pub.session_send;

 evtimer_reuse(ws_pub.pong_timer,@pool,@_pong_pub,arg);
 evtimer_add  (ws_pub.pong_timer,_pong_kd);

end;

function _gen_nonce:RawByteString;
var
 Context:TMTRandomContext;
 _nonce:array[0..5] of DWORD;
begin
 Context:=Default(TMTRandomContext);
 RandomInit(Context);
 _nonce[0]:=Random(Context,high(DWORD)-2)+1;
 _nonce[1]:=Random(Context,high(DWORD)-2)+1;
 _nonce[2]:=Random(Context,high(DWORD)-2)+1;
 _nonce[3]:=Random(Context,high(DWORD)-2)+1;
 _nonce[4]:=Random(Context,high(DWORD)-2)+1;
 _nonce[5]:=Random(Context,high(DWORD)-2)+1;
 SetLength(Result,32);
 base64encode(@_nonce,SizeOf(_nonce),PAnsiChar(Result),32);
 SetLength(Result,30);
end;

function Tws_pub.session_reply:integer;
var
 S:RawByteString;

begin
 Result:=inherited;

 reconnect:=true;

 fpWebsocket_session_new(ws_session,ws_handshake);
 fpWebsocket_session_set_user_data(ws_session,Pointer(self));
 fpWebsocket_session_set_message_cb(ws_session,@_on_message_pub_cb);

 fpWebsocket_handshake_del(ws_handshake);
 ws_handshake:=nil;

 fpWebsocket_session_submit_text(ws_session,PAnsiChar(TW_PING),Length(TW_PING));

 ping_timer:=evtimer_new(@pool,@_ping_pub,Pointer(Self));
 evtimer_add(ping_timer,ping_kd);

 nonce:=_gen_nonce;

 S:='{"type":"LISTEN","nonce":"'+nonce+'",'+
    '"data":{"topics":["community-points-channel-v1.'+chat_id+'"],'+
    '"auth_token":"'+oAuth+'"}}';
 fpWebsocket_session_submit_text(ws_session,PAnsiChar(S),Length(S));

end;

{

*
{"type":"RESPONSE","error":"","nonce":"0DttxLNIP5LdhN9LMJy69AIPJaKR6S"}
*
{"type":"RESPONSE","error":"","nonce":"1DttxLNIP5LdhN0LMJy69AIPJaKR6s"}
*
{"type":"MESSAGE","data":{"topic":"community-points-user-v1.436730045","message"
:"{\"type\":\"global-last-viewed-content-updated\",\"data\":{\"timestamp\":\"202
0-07-07T13:49:07.707593752Z\",\"global_last_viewed_content\":{\"user_id\":\"4367
30045\",\"last_viewed_content\":[{\"content_type\":\"AUTOMATIC_REWARD\",\"conten
t_id\":\"SINGLE_MESSAGE_BYPASS_SUB_MODE\",\"last_viewed_at\":\"2020-07-07T13:49:
07.701959338Z\"},{\"content_type\":\"AUTOMATIC_REWARD\",\"content_id\":\"SEND_HI
GHLIGHTED_MESSAGE\",\"last_viewed_at\":\"2020-07-07T13:49:07.701959338Z\"},{\"co
ntent_type\":\"AUTOMATIC_REWARD\",\"content_id\":\"RANDOM_SUB_EMOTE_UNLOCK\",\"l
ast_viewed_at\":\"2020-07-07T13:49:07.701959338Z\"},{\"content_type\":\"AUTOMATI
C_REWARD\",\"content_id\":\"CHOSEN_SUB_EMOTE_UNLOCK\",\"last_viewed_at\":\"2020-
07-07T13:49:07.701959338Z\"},{\"content_type\":\"AUTOMATIC_REWARD\",\"content_id
\":\"CHOSEN_MODIFIED_SUB_EMOTE_UNLOCK\",\"last_viewed_at\":\"2020-07-07T13:49:07
.701959338Z\"}]}}}"}}
*
{"type":"MESSAGE","data":{"topic":"community-points-user-v1.436730045","message"
:"{\"type\":\"channel-last-viewed-content-updated\",\"data\":{\"timestamp\":\"20
20-07-07T13:49:07.707593752Z\",\"channel_last_viewed_content\":{\"user_id\":\"43
6730045\",\"channel_id\":\"54742538\",\"last_viewed_content\":[{\"content_type\"
:\"AUTOMATIC_REWARD\",\"last_viewed_at\":\"2020-07-07T13:49:07.701959338Z\"},{\"
content_type\":\"CUSTOM_REWARD\",\"last_viewed_at\":\"2020-07-07T13:49:07.701959
338Z\"}]}}}"}}
*
{"type":"MESSAGE","data":{"topic":"community-points-user-v1.436730045","message"
:"{\"type\":\"global-last-viewed-content-updated\",\"data\":{\"timestamp\":\"202
0-07-07T13:49:18.007657183Z\",\"global_last_viewed_content\":{\"user_id\":\"4367
30045\",\"last_viewed_content\":[{\"content_type\":\"AUTOMATIC_REWARD\",\"conten
t_id\":\"SINGLE_MESSAGE_BYPASS_SUB_MODE\",\"last_viewed_at\":\"2020-07-07T13:49:
17.999890913Z\"},{\"content_type\":\"AUTOMATIC_REWARD\",\"content_id\":\"SEND_HI
GHLIGHTED_MESSAGE\",\"last_viewed_at\":\"2020-07-07T13:49:17.999890913Z\"},{\"co
ntent_type\":\"AUTOMATIC_REWARD\",\"content_id\":\"RANDOM_SUB_EMOTE_UNLOCK\",\"l
ast_viewed_at\":\"2020-07-07T13:49:17.999890913Z\"},{\"content_type\":\"AUTOMATI
C_REWARD\",\"content_id\":\"CHOSEN_SUB_EMOTE_UNLOCK\",\"last_viewed_at\":\"2020-
07-07T13:49:17.999890913Z\"},{\"content_type\":\"AUTOMATIC_REWARD\",\"content_id
\":\"CHOSEN_MODIFIED_SUB_EMOTE_UNLOCK\",\"last_viewed_at\":\"2020-07-07T13:49:17
.999890913Z\"}]}}}"}}
*
{"type":"MESSAGE","data":{"topic":"community-points-user-v1.436730045","message"
:"{\"type\":\"channel-last-viewed-content-updated\",\"data\":{\"timestamp\":\"20
20-07-07T13:49:18.007657183Z\",\"channel_last_viewed_content\":{\"user_id\":\"43
6730045\",\"channel_id\":\"54742538\",\"last_viewed_content\":[{\"content_type\"
:\"AUTOMATIC_REWARD\",\"last_viewed_at\":\"2020-07-07T13:49:17.999890913Z\"},{\"
content_type\":\"CUSTOM_REWARD\",\"last_viewed_at\":\"2020-07-07T13:49:17.999890
913Z\"}]}}}"}}
*
{"type":"MESSAGE","data":{"topic":"community-points-user-v1.436730045","message"
:"{\"type\":\"points-spent\",\"data\":{\"timestamp\":\"2020-07-07T13:49:24.01579
8844Z\",\"balance\":{\"user_id\":\"436730045\",\"channel_id\":\"54742538\",\"bal
ance\":87212}}}"}}
*
{"type":"MESSAGE","data":{"topic":"community-points-user-v1.436730045","message"
:"{\"type\":\"points-earned\",\"data\":{\"timestamp\":\"2020-07-07T13:49:44.0609
0841Z\",\"channel_id\":\"54742538\",\"point_gain\":{\"user_id\":\"436730045\",\"
channel_id\":\"54742538\",\"total_points\":12,\"baseline_points\":10,\"reason_co
de\":\"WATCH\",\"multipliers\":[{\"reason_code\":\"SUB_T1\",\"factor\":0.2}]},\"
balance\":{\"user_id\":\"436730045\",\"channel_id\":\"54742538\",\"balance\":872
24}}}"}}
*


}

//{"type":"LISTEN","nonce":"0DttxLNIP5LdhN9LMJy69AIPJaKR6S","data":{"topics":["community-points-user-v1.436730045"],"auth_token":"1892vwii6x81tyd9tz0r1m3tcrmnod"}}

procedure Tws_pub.Clear;
begin
 inherited;
 oAuth  :='';
 chat_id:='';
 nonce  :='';
 evtimer_del(ping_timer);
 evtimer_del(pong_timer);
 ping_timer:=nil;
 pong_timer:=nil;
 FreeAndNil(msg_buf);
end;

Destructor Tws_pub.Destroy;
begin
 if Self=ws_pub then ws_pub:=nil;
 inherited;
end;


 Procedure client_eventcb(bev:Pbufferevent;events:SizeUInt;ctx:pointer); forward;

 Constructor THttpClient.Create_hostname(ssl_ctx:Pssl_ctx;family:Integer;hostname:PAnsiChar;port:Word);
 Var
  FSSL:PSSL;
 begin
  Log(irc_log,0,['Create:',hostname,':',port]);

  if Assigned(ssl_ctx) then
  begin
   FSSL:=create_ssl(ssl_ctx);
   Log(irc_log,1,'Create SSL');
   SSL_set_hostflags(FSSL, X509_CHECK_FLAG_NO_PARTIAL_WILDCARDS);
   SSL_set1_host(FSSL,PByte(hostname));
  end else
  begin
   FSSL:=nil;
   Log(irc_log,1,'Create');
  end;

  if Assigned(ssl_ctx) then
  begin
   bev:=bufferevent_openssl_socket_new(
      @pool, INVALID_SOCKET, FSSL, BUFFEREVENT_SSL_CONNECTING);
  end else
  begin
   bev:=bufferevent_socket_new(@pool,INVALID_SOCKET);
  end;

  bufferevent_setcb(bev,@client_eventcb,Pointer(Self));

  if not bufferevent_socket_connect_hostname(bev,family,hostname,port) then
  begin
   bufferevent_free(bev);
   bev:=nil;
   Log(irc_log,1,'error:bufferevent_socket_connect_hostname');
  end;
 end;

 Destructor THttpClient.Destroy;
 Var
  FSSL:PSSL;
 begin

  FSSL:=bufferevent_openssl_get_ssl(bev);

  if Assigned(Fssl) then
  begin
   //SSL_shutdown(Fssl);
   Log(irc_log,1,'Destroy SSL:');
  end else
  begin
   Log(irc_log,1,'Destroy:');
  end;
  //bufferevent_disable(bev,EV_READ or EV_WRITE);
  bufferevent_free(bev);
  bev:=nil;
  Case http2 of
   True :nghttp2_session_del(session);
   False:fphttp1_session_del(session);
  end;
  session:=nil;
  FreeAndNil(FQueue);
  inherited;
 end;

 function THttpClient.get_stream_user_data(stream_id:PtrUInt):THttpStream;
 begin
  if http2 then
  begin
   Pointer(Result):=nghttp2_session_get_stream_user_data(session,stream_id);
  end else
  begin
   Pointer(Result):=fphttp1_session_get_stream_user_data(session,stream_id);
  end;
 end;

 procedure THttpClient.set_stream_user_data(stream_id:PtrUInt;stream_data:THttpStream);
 begin
  if http2 then
  begin
   nghttp2_session_set_stream_user_data(session,stream_id,stream_data);
  end else
  begin
   fphttp1_session_set_stream_user_data(session,stream_id,stream_data);
  end;
 end;

 procedure THttpClient.delete_stream_data(stream_data:THttpStream);
 begin
  stream_data.Free;
 end;

 function THttpClient.stream_get_state(stream_id:PtrUInt):Tnghttp2_stream_proto_state;
 begin
  if http2 then
  begin
   Result:=nghttp2_stream_get_state(nghttp2_session_find_stream(session,stream_id));
  end else
  begin
   Result:=fphttp1_stream_get_state(fphttp1_session_find_stream(session,stream_id));
  end;
 end;

 function THttpClient.ALPN:Boolean;
 Var
  FSSL:PSSL;
  _alpn:Pointer;
  _alpnlen:size_t;
  i:Integer;
 begin
  Result:=True;
  FSSL:=bufferevent_openssl_get_ssl(bev);
  if Assigned(FSSL) then
  begin
   _alpn:=nil;
   _alpnlen:=0;
   SSL_get0_next_proto_negotiated(FSSL,@_alpn,@_alpnlen);
   if (_alpn=nil) then
   begin
    SSL_get0_alpn_selected(FSSL,@_alpn,@_alpnlen);
   end;
   if (_alpn<>nil) then
   begin
    Log(irc_log,0,['ALPN Select:',GetStr(_alpn,_alpnlen)]);
    Case LowerCase(GetStr(_alpn,_alpnlen)) of
     'http/1.1':http2:=false;
           'h2':http2:=true;
     else Exit(false);
    end;
   end else
   begin
    Log(irc_log,1,'ALPN not Select');
    Exit(False);
   end;
   Log(irc_log,0,['SSL_version:',PChar(SSL_get_version(FSSL)),' SSL_cipher:',PChar(SSL_get_cipher_name(FSSL))]);
   i:=SSL_get_verify_result(FSSL);
   if (i<>0) then
   begin
    Log(irc_log,i,'Error [SSL_get_verify_result]');
    Exit(False);
   end;
   if (SSL_get0_peername(FSSL)=nil) then
   begin
    Log(irc_log,i,'Error [SSL_get0_peername]');
    Exit(False);
   end;
  end;
 end;

 function THttpClient.session_connect:integer;
 Var
  val:integer;
  fs:THandle;
 begin
  Result:=0;
  Fs:=bufferevent_get_fd(bev);
  Log(irc_log,0,['bufferevent_get_fd:',Fs]);
  val:=1;//on
  fpsetsockopt(Fs,IPPROTO_TCP,TCP_NODELAY,@val,sizeof(val));
  SetKeepAlive(Fs,true,30,1,1);
  if not ALPN then Exit(-1);

  Case http2 of
   true :NewSession2;
   false:NewSession1;
  end;

  if Assigned(FQueue) then
   While (FQueue.Count<>0) do
   begin
    submit(THttpStream(FQueue.Items[FQueue.Count-1]));
    FQueue.Delete(FQueue.Count-1);
   end;
  FreeAndNil(FQueue);

  Result:=session_send;
 end;

 procedure THttpClient.NewSession1;
 Var
  iv:Tnghttp2_settings_entry;
  hdrs:array[0..0] of Tnghttp2_nv;
 begin
  fphttp1_session_client_new(session, http1_callbacks, Pointer(self));
  iv.settings_id:=NGHTTP2_SETTINGS_MAX_CONCURRENT_STREAMS;
  iv.value:=100;
  if fphttp1_submit_settings(session,NGHTTP2_FLAG_NONE,@iv,1)<>0 then
  begin
   Log(irc_log,1,'error nghttp2_submit_settings');
  end;
  if Assigned(bufferevent_openssl_get_ssl(bev)) then
  begin
   hdrs[0]:=make_nv_nocopy(NGHTTP2_scheme,Scheme_https);
  end else
  begin
   hdrs[0]:=make_nv_nocopy(NGHTTP2_scheme,Scheme_http);
  end;
  fphttp1_submit_headers(session,0,0,nil,@hdrs,Length(hdrs),nil);
 end;

 procedure THttpClient.NewSession2;
 Var
  iv:Tnghttp2_settings_entry;
 begin
  nghttp2_session_client_new(session, http2_callbacks, Pointer(self));
  iv.settings_id:=NGHTTP2_SETTINGS_MAX_CONCURRENT_STREAMS;
  iv.value:=100;
  if nghttp2_submit_settings(session,NGHTTP2_FLAG_NONE,@iv,1)<>0 then
  begin
   Log(irc_log,1,'error nghttp2_submit_settings');
  end;
 end;

 function read_callback(session: pnghttp2_session;
     stream_id: int32; buf: puint8; length: size_t;
     data_flags: puint32; source: pnghttp2_data_source; user_data: Pointer): ssize_t; cdecl;
 begin
  if not Assigned(source^.ptr) then
  begin
   Result:=0;
   data_flags^:=data_flags^ or NGHTTP2_DATA_FLAG_EOF;
  end else
  begin
   Result:=TStream(source^.ptr).Read(buf^,length);
   if Result<=0 then
   begin
    data_flags^:=data_flags^ or NGHTTP2_DATA_FLAG_EOF;
   end;
  end;
 end;

 procedure THttpClient.submit_request(stream_data:THttpStream;nva:Pnghttp2_nv;nvlen:size_t);
 Var
  data_prd:Tnghttp2_data_provider;
  m:integer;
 begin
  if not Assigned(stream_data) then Exit;

  if Assigned(stream_data.FSends) then
  begin
   data_prd.source.ptr:=Pointer(stream_data.FSends);
   data_prd.read_callback:=@read_callback;
  end else
  begin
   data_prd:=Default(Tnghttp2_data_provider);
  end;

  if http2 then
  begin
   m:=nghttp2_submit_request(session,nil,nva,nvlen,@data_prd,Pointer(stream_data));
  end else
  begin
   m:=fphttp1_submit_request(session,nil,nva,nvlen,@data_prd,Pointer(stream_data));
  end;

  if m<0 then
  begin
   Log(irc_log,1,['error nghttp2_submit_request',m]);
  end else
  begin
   stream_data.stream_id:=m;
  end;
 end;

 procedure THttpClient.submit(stream_data:THttpStream);
 const
  M_scheme:array[boolean] of PChar=(Scheme_http,Scheme_https);
 Var
  i,ext_len,hdr_len:SizeUInt;
  hdrs:Pnghttp2_nv;
 begin
  if not Assigned(stream_data) then Exit;

  stream_data.FClientData:=Self;

  if session=nil then
  begin
   if not Assigned(FQueue) then
   begin
    FQueue:=TFPList.Create;
   end;
   FQueue.Add(stream_data);
   Exit;
  end;

  ext_len:=Length(stream_data.headers) div 2;
  hdr_len:=ext_len+4;

  hdrs:=AllocMem(SizeOf(Tnghttp2_nv)*hdr_len);

  hdrs[0]:=make_nv_nocopy_name(NGHTTP2_method,PChar(stream_data.Method));
  hdrs[1]:=make_nv_nocopy(NGHTTP2_scheme,M_scheme[bufferevent_openssl_get_ssl(bev)<>nil]);

  hdrs[2]:=make_nv_nocopy_name(NGHTTP2_authority,PChar(stream_data.Host));
  hdrs[3]:=make_nv_nocopy_name(NGHTTP2_path     ,PChar(stream_data.Path));

  if ext_len>0 then
   For i:=0 to ext_len-1 do
   begin
    hdrs[4+i]:=MAKE_NV(PChar(stream_data.headers[i*2]),PChar(stream_data.headers[i*2+1]));
   end;

  submit_request(stream_data,hdrs,hdr_len);
  FreeMem(hdrs);

  session_send;
 end;

 procedure THttpClient.terminate;
 begin
  if http2 then
  begin
   nghttp2_session_terminate_session(session,0);
  end else
  begin
   fphttp1_session_terminate_session(session,0);
  end;
 end;

 procedure THttpClient.print_err;
 Var
  FSSL:PSSL;
 begin
  FSSL:=bufferevent_openssl_get_ssl(bev);
  if Assigned(FSSL) then
  begin
   Log(irc_log,SSL_get_error(FSSL,0),'[SSL_get_error]');
  end;
 end;

 procedure THttpStream.on_begin_data;
 begin
 end;

 procedure THttpStream.on_begin_headers(cat:Tnghttp2_headers_category);
 begin
 end;

 procedure THttpStream.on_recv_data(data:pointer;len:size_t);
 begin
  if Assigned(FRecvs) then
  begin
   FRecvs.Write(data^,len);
  end;
 end;

 procedure THttpStream.on_headers(Const name,value:RawByteString;cat:Tnghttp2_headers_category);
 begin
  case name of
   ':status':status:=value;
  end;
 end;

 procedure THttpStream.on_end_headers;
 begin
 end;

 procedure THttpStream.on_end_stream;
 begin
  if Assigned(FOnEndStream) then
   FOnEndStream(Self);
 end;

 function THttpClient.session_send:integer;
 Var
  data_ptr:Puint8;
  output:Pevbuffer;
 begin
  if session=nil then Exit(0);

  output:=bufferevent_get_output(bev);

  repeat
   data_ptr:=nil;
   if http2 then
   begin
    Result:=nghttp2_session_mem_send(session,data_ptr);
   end else
   begin
    Result:=fphttp1_session_mem_send(session,data_ptr);
   end;
   if Result>0 then
   begin

    evbuffer_add(output,data_ptr,Result);

    if (evbuffer_get_length(output)>=OUTPUT_WOULDBLOCK_THRESHOLD) then
    begin
     Result:=0;
    end;

   end else
   if Result<>0 then
   begin
    //Writeln('session_send(',Result,')');
   end;

  until (Result<=0);

  bufferevent_write(bev);
 end;

 function THttpClient.session_recv:integer;
 Var
  datapos,datalen:sizeUInt;
  readlen:ssize_t;
  input:Pevbuffer;
  data:PByte;

  vec:Piovec;

 begin
  Result:=0;

  input:=bufferevent_get_input(bev);
  datalen:=evbuffer_get_length(input);

  if datalen=0 then Exit;

  datapos:=datalen;

  While (datapos<>0) do
  begin

   data:=nil;

   vec:=evbuffer_peek(input);
   if not Assigned(vec) then Break;

   if (vec^.len>=datapos) then
   begin
    datapos:=0;
   end else
   begin
    datapos:=datapos-vec^.len;
   end;

   datalen:=vec^.len;
   data:=iovec_getdata(vec);

   if (session=nil) then
   begin
    ////
   end;

   if http2 then
   begin
    readlen:=nghttp2_session_mem_recv(session,data,datalen);
   end else
   begin
    readlen:=fphttp1_session_mem_recv(session,data,datalen);
   end;
   if readlen<0 then
   begin
    Log(irc_log,1,['error session_mem_recv(',readlen,')']);
    exit(-1);
   end else
   begin
    evbuffer_drain(input,readlen);
   end;

  end;

  Result:=session_send;
 end;

 procedure THttpClient.session_shutdown(how:Longint);
 begin
  if Assigned(bufferevent_openssl_get_ssl(bev)) then
  begin
   bufferevent_openssl_shutdown(bev);
  end else
  begin
   Log(irc_log,0,['bufferevent_shutdown:',bufferevent_shutdown(bev,0)]);
  end;
 end;

 Procedure client_eventcb(bev:Pbufferevent;events:SizeUInt;ctx:pointer);
 Var
  ClientData:THttpClient;
 begin
  ClientData:=THttpClient(ctx);
  if ClientData=nil then Exit;

  if (events and (BEV_EVENT_EOF or BEV_EVENT_ERROR or BEV_EVENT_TIMEOUT))<>0 then
  begin
   Log(irc_log,1,'BEV_EVENT_ERROR');
   ClientData.print_err;
   FreeAndNil(ClientData);
   Exit;
  end;

  if (events and BEV_EVENT_READING)<>0 then
  begin
   if ClientData.session_recv<0 then
   begin
    ClientData.print_err;
    ClientData.Free;
   end;
  end;

  if (events and BEV_EVENT_WRITING)<>0 then
  begin
   if ClientData.session_send<0 then
   begin
    ClientData.print_err;
    ClientData.session_shutdown(1);
   end;
  end;

  if (events and BEV_EVENT_CONNECTED)<>0 then
  begin
   Log(irc_log,1,['BEV_EVENT_CONNECTED:',bufferevent_get_fd(bev),':',GetThreadID]);
   if ClientData.session_connect<0 then
   begin
    ClientData.print_err;
    //bufferevent_close(ClientData.bev);
    //ClientData.session_shutdown(2);
    ClientData.Free;
   end;
  end;

 end;

 function on_frame_recv_callback(session: pnghttp2_session;
              frame:pnghttp2_frame; user_data: Pointer): Integer; cdecl;
 Var
  ClientData:THttpClient;
  stream_data:THttpStream;

 begin
  ClientData:=THttpClient(user_data);

  //if frame^.hd.stream_id<>0 then
  begin
   if (frame^.hd.flags and NGHTTP2_FLAG_END_HEADERS)<>0 then
   begin
    Log(irc_log,0,['NGHTTP2_FLAG_END_HEADERS:',frame^.hd.stream_id]);
    stream_data:=ClientData.get_stream_user_data(frame^.hd.stream_id);
    if Assigned(stream_data) then
     stream_data.on_end_headers;
   end;

   if (frame^.hd.flags and NGHTTP2_FLAG_END_STREAM)<>0 then
   begin
    Log(irc_log,0,['NGHTTP2_FLAG_END_STREAM:',frame^.hd.stream_id]);
    stream_data:=ClientData.get_stream_user_data(frame^.hd.stream_id);
    if Assigned(stream_data) then
     stream_data.on_end_stream;
   end;
  end;

  Result:=0;
 end;

 function on_stream_close_callback(session: pnghttp2_session;
   stream_id: int32; error_code: uint32; user_data: Pointer): Integer; cdecl;
 Var
  ClientData:THttpClient;
  stream_data:THttpStream;

 begin
  Log(irc_log,0,['stream_close:',stream_id,':',error_code]);
  ClientData:=THttpClient(user_data);

  stream_data:=ClientData.get_stream_user_data(stream_id);
  ClientData.set_stream_user_data(stream_id,nil);

  if stream_data=nil then Exit(0);

  ClientData.delete_stream_data(stream_data);
  Result:=0;
 end;


 function on_header_callback(session: pnghttp2_session; frame: pnghttp2_frame;
   name: puint8; namelen: size_t; value: puint8; valuelen: size_t;
   flags: uint8; user_data: Pointer): Integer; cdecl;
 Var
  ClientData:THttpClient;
  stream_data:THttpStream;

 begin
  Result:=0;
  ClientData:=THttpClient(user_data);

  case frame^.hd._type of
   NGHTTP2_HEADERS:
   begin
    stream_data:=ClientData.get_stream_user_data(frame^.hd.stream_id);

    //Writeln(GetStr(Pointer(name),namelen),' ',GetStr(Pointer(value),valuelen));

    if (stream_data<>nil) then
    begin
     stream_data.on_headers(GetStr(Pointer(name),namelen),GetStr(Pointer(value),valuelen),frame^.headers.cat);
    end;

   end;
  end;


 end;

 function on_begin_headers_callback(
  session: pnghttp2_session;
  frame:pnghttp2_frame;
  user_data:Pointer
  ):integer; cdecl;
 Var
  ClientData:THttpClient;
  stream_data:THttpStream;

 begin

  Result:=0;
  ClientData:=THttpClient(user_data);

  stream_data:=ClientData.get_stream_user_data(frame^.hd.stream_id);

  if (stream_data<>nil) then
   case frame^.hd._type of
    NGHTTP2_DATA   :stream_data.on_begin_data;
    NGHTTP2_HEADERS:stream_data.on_begin_headers(frame^.headers.cat);
   end;

 end;

 function on_recv_data(session:Pnghttp2_session;
               flags:uint8; stream_id:int32; data:Puint8; len:size_t;
               user_data:pointer):integer;cdecl;
 Var
  ClientData:THttpClient;
  stream_data:THttpStream;
 begin
  Result:=len;

  ClientData:=THttpClient(user_data);
  stream_data:=ClientData.get_stream_user_data(stream_id);

  if Assigned(stream_data) then
  begin
   stream_data.on_recv_data(data,len);
  end;
 end;

 Procedure Init_Callbacks;
 begin
  if http1_callbacks=nil then
  begin
   fphttp1_session_callbacks_new(http1_callbacks);

   fphttp1_session_callbacks_set_on_frame_recv_callback(http1_callbacks,@on_frame_recv_callback);
   fphttp1_session_callbacks_set_on_stream_close_callback(http1_callbacks, @on_stream_close_callback);
   fphttp1_session_callbacks_set_on_header_callback(http1_callbacks, @on_header_callback);
   fphttp1_session_callbacks_set_on_begin_headers_callback(http1_callbacks,@on_begin_headers_callback);
   fphttp1_session_callbacks_set_on_data_chunk_recv_callback(http1_callbacks,@on_recv_data);

   fphttp1_session_callbacks_set_on_invalid_header_callback(http1_callbacks,@on_header_callback);

   //fphttp1_session_callbacks_set_send_data_callback(http1_callbacks,@send_data_nocopy_callback);

  end;

  /////////////////////

  if http2_callbacks=nil then
  begin
   nghttp2_session_callbacks_new(http2_callbacks);

   nghttp2_session_callbacks_set_on_frame_recv_callback(http2_callbacks,@on_frame_recv_callback);
   nghttp2_session_callbacks_set_on_stream_close_callback(http2_callbacks, @on_stream_close_callback);
   nghttp2_session_callbacks_set_on_header_callback(http2_callbacks, @on_header_callback);
   nghttp2_session_callbacks_set_on_begin_headers_callback(http2_callbacks,@on_begin_headers_callback);
   nghttp2_session_callbacks_set_on_data_chunk_recv_callback(http2_callbacks,@on_recv_data);

   nghttp2_session_callbacks_set_on_invalid_header_callback(http2_callbacks,@on_header_callback);



  end;
  //nghttp2_session_callbacks_set_send_data_callback(http2_callbacks,@send_data_nocopy_callback);

 end;

Constructor THttpStream.Create;
begin
 inherited;
 Method:='GET';
end;

Procedure THttpStream.SetUrl(const url:RawByteString);
var
 URI:TURI;
 q:RawByteString;
begin
 URI:=parse_uri(url);
 Host:=URI.GetHost;
 Path:=URI.GetPath(true);
 if Path='' then Path:='/';
 q:=URI.getQuery(true);
 if (q<>'') then
 begin
  Path:=Path+'?'+q;
 end;
end;

Procedure THttpStream.AddHeader(Const name,value:RawByteString);
var
 i:SizeInt;
begin
 i:=Length(headers);
 SetLength(headers,i+2);
 headers[i]:=name;
 headers[i+1]:=value;
end;

Procedure THttpStream.AddStdHdr;
begin
 AddHeader('User-Agent','Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:82.0) Gecko/20100101 Firefox/82.0');
 AddHeader('Accept','text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8');
 AddHeader('Accept-Language','ru-RU,ru;q=0.8,en-US;q=0.5,en;q=0.3');
 AddHeader('Upgrade-Insecure-Requests','1');
 AddHeader('Pragma','no-cache');
 AddHeader('Cache-Control','no-cache');
end;

Constructor THttpStream2Mem.Create;
begin
 inherited;
 FRecvs:=TMemoryStream.Create;
end;

Destructor  THttpStream2Mem.Destroy;
begin
 FreeAndNil(FRecvs);
 inherited;
end;

Procedure TProgXCHG.Clear;
begin
 Self:=Default(TProgXCHG);
 PFA:=@FA;
 PFB:=@FB;
end;

Procedure TProgXCHG.Write(_Load,_Size:QWORD);
Var
 P:PBlock;
begin
 P:=System.InterLockedExchange(PFA,nil);
 if P<>nil then
 begin
  P^.FTime:=GetTickCount64;
  P^.Load:=_Load;
  P^.Size:=_Size;
  P:=System.InterLockedExchange(PFB,P);
  if P<>nil then
  begin
   P:=System.InterLockedExchange(PFA,P);
  end;
 end;
end;

function  TProgXCHG.Read(Offset:QWORD):TProgInfo;
Var
 P:PBlock;
 T:TBlock;
 Q:QWORD;
 N:QWORD;

begin
 P:=System.InterLockedExchange(PFB,nil);
 if P<>nil then
 begin
  T:=P^;
  P:=System.InterLockedExchange(PFB,P);
  if P<>nil then
  begin
   P:=System.InterLockedExchange(PFA,P);
  end;

  if FTime=0 then
  begin
   prev.Speed:=0;
  end else
  begin
   prev.Speed:=0;
   Q:=T.FTime-FTime;
   if T.Load<prev.Load then
    N:=0
   else
    N:=T.Load-prev.Load;
   if (Q>0) then
   begin
    prev.Speed:=N*1000 div Q;
   end;
  end;

  FTime:=T.FTime;

  prev.Load:=T.Load;
  prev.Size:=T.Size;
 end;

 Result:=prev;

 if offset>0 then
 begin
  if (Result.Size>0) then
   Result.Size:=offset+Result.Size;
  if (Result.Load>=0) then
   Result.Load:=offset+Result.Load;
 end;

end;

procedure THttpStream2File.on_headers(Const name,value:RawByteString;cat:Longint);
begin
 inherited;
 case name of
  'content-length':
  begin
   Flen:=StrToQWordDef(value,0);
   Prog.Write(0,Flen);
  end;
 end;
end;

procedure THttpStream2File.on_recv_data(data:pointer;len:size_t);
begin
 inherited;
 Prog.Write(FRecvs.Size,Flen);
end;

Constructor THttpStream2File.Create(const AFileName:string;Mode:Word);
begin
 inherited Create;
 Prog.Clear;
 FRecvs:=TFileStream.Create(AFileName,Mode);
end;

Destructor  THttpStream2File.Destroy;
begin
 FreeAndNil(FRecvs);
 inherited;
end;

 function replyConnect(var ClientData:THttpClient;AClass:THttpClientClass;Const Path:RawByteString):Boolean;
 Var
  URI:TURI;
  ctx:PSSL_CTX;
  port:Word;

 begin
  Result:=False;
  Init_Callbacks;

  URI:=parse_uri(Path);

  if not Assigned(ClientData) then
  begin
   ctx:=nil;
   port:=URI.GetPort;
   Case LowerCase(URI.getProtocol()) of
    '',
    'http' :if port=0 then port:=80;
    'https':begin
             if port=0 then port:=443;
             if https_ctx=nil then https_ctx:=create_ssl_ctx(true);
             ctx:=https_ctx;
            end;
   end;
   Log(irc_log,0,['CONNECT TO:',URI.GetHost+':'+URI.GetPath,':',port]);
   ClientData:=AClass.Create_hostname(ctx,AF_INET,PAnsiChar(URI.GetHost),port);
   Result:=(ClientData.bev<>nil);
   if not Result then
   begin
    Log(irc_log,1,['HOST NOT FOUND:',URI.GetHost+':'+URI.GetPath,':',port]);
    FreeAndNil(ClientData);
   end;
  end;

 end;

end.

