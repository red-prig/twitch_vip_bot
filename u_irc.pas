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
 phttp2_stream_data=^http2_stream_data;
 http2_stream_data=record
  request_path:PChar;
  stream_id:int32;
  FStream:TStream;
 end;

 TClientData=class
  //private
   bev:Pbufferevent;
   session:pnghttp2_session;
   http2:Boolean;
   Method,Host,Path:RawByteString;
   headers:array of RawByteString;
   Sends,Recvs:TStream;
  //public
  Constructor Create_hostname(ssl_ctx:Pssl_ctx;family:Integer;hostname:PAnsiChar;port:Word);

  Destructor Destroy; override;

  function  get_stream_user_data(stream_id:PtrUInt):phttp2_stream_data;
  procedure set_stream_user_data(stream_id:PtrUInt;stream_data:phttp2_stream_data);
  function  create_stream_data(stream_id:int32):Phttp2_stream_data; virtual;
  procedure delete_stream_data(stream_data:Phttp2_stream_data); virtual;

  function  stream_get_state(stream_id:PtrUInt):Tnghttp2_stream_proto_state;

  Procedure ALPN;
  procedure NewSession1;
  procedure NewSession2;
  function  session_connect:integer;
  function  session_send:integer;
  function  session_recv:integer;
  procedure session_shutdown(how:Longint);
  procedure submit_request(nva:Pnghttp2_nv;nvlen:size_t;S:TStream);
  procedure submit_path;
  procedure on_headers(stream_data:phttp2_stream_data;Const name,value:RawByteString); virtual;
  procedure on_end_stream(stream_data:phttp2_stream_data); virtual;
 end;

procedure reply_irc_Connect(const login,oAuth,chat,chat_id:RawByteString);
procedure reply_irc_Disconnect;
procedure reply_irc_msg(const msg:RawByteString);

implementation

uses
 Main;

type
 irc_log=class(app_logic);

var
 app_ctx:PSSL_CTX;
 msg_send_buf:Pevbuffer;

 http1_callbacks,http2_callbacks:pnghttp2_session_callbacks;

Const
 SSL_TLSEXT_ERR_OK = 0;
 SSL_TLSEXT_ERR_ALERT_WARNING = 1;
 SSL_TLSEXT_ERR_ALERT_FATAL = 2;
 SSL_TLSEXT_ERR_NOACK = 3;

Const
 Protos_NPN:string=#8'http/1.1';
 Protos_ALPN:string=#8'http/1.1'{#2'h2'};

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

Function create_ssl_ctx:PSSL_CTX;
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

 SSL_CTX_set_alpn_protos(Result,PByte(PChar(Protos_ALPN)),Length(Protos_ALPN));

 SSL_CTX_set_options(Result,
                     SSL_OP_ALL or
                     SSL_OP_NO_COMPRESSION or
                     SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION or
                     SSL_MODE_RELEASE_BUFFERS);

end;

Procedure Websocket_eventcb(bev:Pbufferevent;events:SizeUInt;ctx:pointer); forward;

type
 TWebsocketData=class
   bev:Pbufferevent;
   ws_handshake:PfpWebsocket_handshake;
   ws_session:PfpWebsocket_session;
   time_kd:QWORD;
   url:RawByteString;
   Constructor Create_hostname(ssl_ctx:Pssl_ctx;family:Integer;hostname:PAnsiChar;port:Word);
   Destructor  Destroy; override;
   function    session_recv:integer;
   function    session_send:integer;
   Procedure   ALPN;
   procedure   NewSession;
   function    session_connect:integer; virtual;
   function    session_reply:integer;   virtual;
   procedure   session_reconnect;       virtual;
 end;

 Tws_irc=class(TWebsocketData)
  reconnect:Boolean;
  msg_timer:Ptimer;
  login,oAuth,chat:RawByteString;
  msg_buf:RawByteString;
  function    session_reply:integer;   override;
  procedure   session_reconnect;       override;
  Destructor  Destroy; override;
 end;

 Tws_pub=class(TWebsocketData)
  reconnect:Boolean;
  msg_timer:Ptimer;
  oAuth,chat_id:RawByteString;
  nonce:RawByteString;
  msg_buf:TMemoryStream;
  function    session_reply:integer;   override;
  procedure   session_reconnect;       override;
  Destructor  Destroy; override;
 end;

var
 ws_irc:Tws_irc;
 ws_pub:Tws_pub;

function create_ssl(ssl_ctx:Pssl_ctx):PSSL;
begin
 Result:=SSL_new(ssl_ctx);
 if not Assigned(Result) then
 begin
  Log(irc_log,1,['Could not create SSL/TLS session object: ',ERR_error_string(ERR_get_error(),nil)]);
 end;
end;

Constructor TWebsocketData.Create_hostname(ssl_ctx:Pssl_ctx;family:Integer;hostname:PAnsiChar;port:Word);
Var
 FSSL:PSSL;
begin
 time_kd:=1500000;

 Log(irc_log,0,['Create:',hostname,':',port]);

 if Assigned(ssl_ctx) then
 begin
  FSSL:=create_ssl(ssl_ctx);
  Log(irc_log,0,'Create SSL');
 end else
 begin
  FSSL:=nil;
  Log(irc_log,0,'Create');
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


 if not bufferevent_socket_connect_hostname(bev,family,hostname,port) then
 begin
  Log(irc_log,1,['error:bufferevent_socket_connect_hostname']);
 end;
end;

Destructor TWebsocketData.Destroy;
begin
 fpWebsocket_handshake_del(ws_handshake);
 fpWebsocket_session_del(ws_session);
 bufferevent_free(bev);
 bev:=nil;
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

Procedure TWebsocketData.ALPN;
Var
 FSSL:PSSL;
 _alpn:Pointer;
 _alpnlen:size_t;
begin
 FSSL:=bufferevent_openssl_get_ssl(bev);
 if Assigned(FSSL) then
 begin
  _alpn:=nil;
  _alpnlen:=0;
  SSL_get0_next_proto_negotiated(FSSL,@_alpn,@_alpnlen);
  if _alpn=nil then
  begin
   SSL_get0_alpn_selected(FSSL,@_alpn,@_alpnlen);
  end;
  if _alpn<>nil then
  begin
   Log(irc_log,0,['ALPN Select:',GetStr(_alpn,_alpnlen)]);
  end;
  Log(irc_log,0,['SSL_version:',PChar(SSL_get_version(FSSL)),' SSL_cipher:',PChar(SSL_get_cipher_name(FSSL))]);
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
 ALPN;
 NewSession;
 session_send;
end;

function TWebsocketData.session_reply:integer;
begin
 Result:=0;
 Log(irc_log,0,'session_reply');
end;

procedure TWebsocketData.session_reconnect;
begin
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
    SessionData.Free;
   end else
    bufferevent_read(bev);
  end;
  if ((events and BEV_EVENT_WRITING)<>0) or
     fpWebsocket_session_want_write(SessionData.ws_session) then
  begin
   if SessionData.session_send<0 then
   begin
    SessionData.Free;
   end;
  end;

  if (events and BEV_EVENT_CONNECTED)<>0 then
  begin
   Log(irc_log,0,['BEV_EVENT_CONNECTED:',bufferevent_get_fd(bev),':',GetThreadID]);
   if SessionData.session_connect<0 then
   begin
    SessionData.Free;
   end;
  end;

 end;

 if (events and (BEV_EVENT_EOF or BEV_EVENT_ERROR or BEV_EVENT_TIMEOUT))<>0 then
 begin
  Log(irc_log,0,['BEV_EVENT_ERROR_WS:',events and BEV_EVENT_EOF<>0]);
  if Assigned(SessionData) then
  begin
   SessionData.session_reconnect;
   FreeAndNil(SessionData);
   Exit;
  end;
 end;

end;

procedure replyConnect_irc(const login,oAuth,chat:RawByteString);
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
         if app_ctx=nil then app_ctx:=create_ssl_ctx;
         ctx:=app_ctx;
        end;
 end;
 Log(irc_log,0,['CONNECT TO:',URI.GetHost+':'+URI.GetPath,':',port]);

 ws_irc:=Tws_irc.Create_hostname(ctx,AF_INET,PAnsiChar(URI.GetHost),port);
 ws_irc.url:=Path;
 ws_irc.login:=login;
 ws_irc.oAuth:=oAuth;
 ws_irc.chat :=chat;
end;

procedure Tws_irc.session_reconnect;
begin
 if reconnect then
  replyConnect_irc(login,oAuth,chat);
end;

Destructor Tws_irc.Destroy;
begin
 if Self=ws_irc then ws_irc:=nil;
 evtimer_del(msg_timer);
 msg_timer:=nil;
 inherited;
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

Procedure _submit_tm(ev:Ptimer;arg:pointer);
var
 v:Piovec;

begin
 v:=evbuffer_pop(msg_send_buf);
 if (v<>nil) then
 begin
  Log(irc_log,0,['<',GetStr(iovec_getdata(v),iovec_getlen(v))]);

  main.push_chat('>'+ws_irc.login+': '+GetStr(iovec_getdata(v),iovec_getlen(v)));

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

procedure _submit_msg(const msg:RawByteString);
//var
// S:RawByteString;
begin
 if msg_send_buf=nil then
 begin
  msg_send_buf:=evbuffer_new;
 end;

 if (msg='') or (ws_irc=nil) then Exit;

 //S:={'@client-nonce='+_gen_nonce+' '+}'PRIVMSG #'+ws_irc.chat+' :'+msg;

 evbuffer_add(msg_send_buf,PAnsiChar(msg),Length(msg));

 if ws_irc.msg_timer=nil then
 begin
  _submit_tm(ws_irc.msg_timer,ws_irc.ws_session);
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
  msg:RawByteString;
  slow:QWORD;
  _mod,_sub:Byte;
  procedure parse(var S:RawByteString);
 end;

procedure Tmsg_parse.parse(var S:RawByteString);
var
 v,n,
 param:RawByteString;
 i:SizeInt;
begin
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
   user:=Copy(user,i+2,Length(user)-(i+1));
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
   'subscriber':_sub:=StrToIntDef(v,0);
   'mod' :_mod:=StrToIntDef(v,0);
   'slow':slow:=StrToQWordDef(v,0);
   'display-name':display_name:=v;
   //Writeln(n,'*',v,'*');
  end;

 until (param='');

end;

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
     'ROOMSTATE':begin
                  if (LowerCase(ws_irc.chat)<>LowerCase(ws_irc.login))
                     and (msg_parse._mod=0)
                     and (msg_parse._sub=0)
                     and (msg_parse.slow<>0) then
                  begin
                   ws_irc.time_kd:=msg_parse.slow*1000000;
                  end;
                 end;

     '001':begin
            main.push_chat('Добро пожаловать в чат!');
            main.push_login;
           end;
     'NOTICE':begin
               main.push_chat(msg_parse.msg);
               case msg_parse.msg of
                'Login authentication failed':
                begin
                 Result:=-1;
                 ws_irc.reconnect:=false;
                end;
               end;
              end;
     'PRIVMSG':begin
                main.push_chat(msg_parse.display_name+':'+msg_parse.msg);
                //Log(irc_log,0,['>',msg_parse.display_name,' (',msg_parse.user,'):',msg_parse.msg]);
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
 r1:RawByteString='CAP REQ :twitch.tv/tags twitch.tv/commands';
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

 fpWebsocket_session_submit_text(ws_session,PAnsiChar(r1),Length(r1));
 S:='PASS oauth:'+oAuth;
 fpWebsocket_session_submit_text(ws_session,PAnsiChar(S),Length(S));
 S:='NICK '+login;
 fpWebsocket_session_submit_text(ws_session,PAnsiChar(S),Length(S));
 S:='USER '+login+' 8 * :'+login;
 fpWebsocket_session_submit_text(ws_session,PAnsiChar(S),Length(S));
 S:='JOIN #'+chat;
 fpWebsocket_session_submit_text(ws_session,PAnsiChar(S),Length(S));
end;

procedure replyConnect_pub(const oAuth,chat_id:RawByteString);
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
         if app_ctx=nil then app_ctx:=create_ssl_ctx;
         ctx:=app_ctx;
        end;
 end;
 Log(irc_log,0,['CONNECT TO:',URI.GetHost+':'+URI.GetPath,':',port]);

 ws_pub:=Tws_pub.Create_hostname(ctx,AF_INET,PAnsiChar(URI.GetHost),port);
 ws_pub.url:=Path;
 ws_pub.oAuth:=oAuth;
 ws_pub.chat_id:=chat_id;
end;

type
 Pirc_Connect=^Tirc_Connect;
 Tirc_Connect=record
  login,oAuth,chat,chat_id:PAnsiChar;
 end;

Procedure _irc_Connect_post(param1:SizeUInt;param2:Pointer);
begin
 case param1 of
  0:begin
     FreeAndNil(ws_irc);
     FreeAndNil(ws_pub);
     With Pirc_Connect(param2)^ do
     begin
      replyConnect_irc(GetStr(login,StrLen(login)),GetStr(oAuth,StrLen(oAuth)),GetStr(chat,StrLen(chat)));
      replyConnect_pub(GetStr(oAuth,StrLen(oAuth)),GetStr(chat_id,StrLen(chat_id)));
      FreeMem(login  );
      FreeMem(oAuth  );
      FreeMem(chat   );
      FreeMem(chat_id);
     end;
     FreeMem(param2);
    end;
  1:begin
     evbuffer_clear(msg_send_buf);
     FreeAndNil(ws_irc);
     FreeAndNil(ws_pub);
    end;
  2:begin
     if ws_irc<>nil then
     begin
      _submit_msg(GetStr(param2,StrLen(param2)));
     end;
     FreeMem(param2);
    end;
 end;
end;

function CopyPchar(Src:PAnsiChar;Len:size_t):PAnsiChar;
begin
 Result:=nil;
 if (Src=nil) or (Len=0) then Exit;
 Result:=GetMem(Len+1);
 Move(Src^,Result^,Len);
 Result[Len]:=#0;
end;

procedure reply_irc_Connect(const login,oAuth,chat,chat_id:RawByteString);
var
 P:Pirc_Connect;
begin
 P:=GetMem(SizeOf(Tirc_Connect));
 P^.login  :=CopyPchar(PAnsiChar(login)  ,Length(login));
 P^.oAuth  :=CopyPchar(PAnsiChar(oAuth)  ,Length(oAuth));
 P^.chat   :=CopyPchar(PAnsiChar(chat)   ,Length(chat));
 P^.chat_id:=CopyPchar(PAnsiChar(chat_id),Length(chat_id));
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
    'PING':begin
            fpWebsocket_session_submit_text(session,PAnsiChar(TW_PONG),Length(TW_PONG));
           end;
    'PONG':begin
            //pong
           end;
    'RESPONSE':if ws_pub.nonce=msg1.Path['nonce'].AsStr then
               begin
                msg:=msg1.Path['error'].AsStr;
                if msg<>'' then Result:=-1;
                if msg='' then msg:='Ожидаем события поинтов!';
                main.push_chat(msg);

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
 ping_kd=4*60*1000000;

Procedure _ping_pub(ev:Ptimer;arg:pointer);
var
 ws_session:PfpWebsocket_session;
 ws_pub:Tws_pub;
begin
 ws_session:=arg;
 fpWebsocket_session_submit_text(ws_session,PAnsiChar(TW_PING),Length(TW_PING));
 evtimer_add(ev,ping_kd);

 Pointer(ws_pub):=fpWebsocket_session_get_user_data(arg);
 ws_pub.session_send;
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

 msg_timer:=evtimer_new(@pool,@_ping_pub,ws_session);
 evtimer_add(msg_timer,ping_kd);

 nonce:=_gen_nonce;

 S:='{"type":"LISTEN","nonce":"'+nonce+'",'+
    '"data":{"topics":["community-points-channel-v1.'+chat_id+'"],'+
    '"auth_token":"'+oAuth+'"}}';
 fpWebsocket_session_submit_text(ws_session,PAnsiChar(S),Length(S));

end;

procedure Tws_pub.session_reconnect;
begin
 replyConnect_pub(oAuth,chat_id);
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

Destructor Tws_pub.Destroy;
begin
 if Self=ws_pub then ws_pub:=nil;
 evtimer_del(msg_timer);
 FreeAndNil(msg_buf);
 inherited;
end;


 Procedure client_eventcb(bev:Pbufferevent;events:SizeUInt;ctx:pointer); forward;

 Constructor TClientData.Create_hostname(ssl_ctx:Pssl_ctx;family:Integer;hostname:PAnsiChar;port:Word);
 Var
  FSSL:PSSL;
 begin
  Log(irc_log,0,['Create:',hostname,':',port]);

  if Assigned(ssl_ctx) then
  begin
   FSSL:=create_ssl(ssl_ctx);
   Log(irc_log,1,'Create SSL');
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
   Log(irc_log,1,'error:bufferevent_socket_connect_hostname');
  end;
 end;

 Destructor TClientData.Destroy;
 Var
  FSSL:PSSL;
 begin

  FreeAndNil(Recvs);

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
  inherited;
 end;

 function TClientData.get_stream_user_data(stream_id:PtrUInt):phttp2_stream_data;
 begin
  if http2 then
  begin
   Result:=nghttp2_session_get_stream_user_data(session,stream_id);
  end else
  begin
   Result:=fphttp1_session_get_stream_user_data(session,stream_id);
  end;
 end;

 procedure TClientData.set_stream_user_data(stream_id:PtrUInt;stream_data:phttp2_stream_data);
 begin
  if http2 then
  begin
   nghttp2_session_set_stream_user_data(session,stream_id,stream_data);
  end else
  begin
   fphttp1_session_set_stream_user_data(session,stream_id,stream_data);
  end;
 end;

 function TClientData.create_stream_data(stream_id:int32):Phttp2_stream_data;
 begin
  Result:=AllocMem(sizeof(http2_stream_data));
  Result^.stream_id:=stream_id;
 end;

 procedure TClientData.delete_stream_data(stream_data:Phttp2_stream_data);
 begin
  FreeMem(stream_data^.request_path);
  FreeAndNil(stream_data^.FStream);
  FreeMem(stream_data);
 end;

 function TClientData.stream_get_state(stream_id:PtrUInt):Tnghttp2_stream_proto_state;
 begin
  if http2 then
  begin
   Result:=nghttp2_stream_get_state(nghttp2_session_find_stream(session,stream_id));
  end else
  begin
   Result:=fphttp1_stream_get_state(fphttp1_session_find_stream(session,stream_id));
  end;
 end;

 Procedure TClientData.ALPN;
 Var
  FSSL:PSSL;
  _alpn:Pointer;
  _alpnlen:size_t;
 begin
  FSSL:=bufferevent_openssl_get_ssl(bev);
  if Assigned(FSSL) then
  begin
   _alpn:=nil;
   _alpnlen:=0;
   SSL_get0_next_proto_negotiated(FSSL,@_alpn,@_alpnlen);
   if _alpn=nil then
   begin
    SSL_get0_alpn_selected(FSSL,@_alpn,@_alpnlen);
   end;
   if _alpn<>nil then
   begin
    Log(irc_log,0,['ALPN Select:',GetStr(_alpn,_alpnlen)]);
    Case LowerCase(GetStr(_alpn,_alpnlen)) of
     'http/1.1':http2:=false;
           'h2':http2:=true;
    end;
   end;
   Log(irc_log,0,['SSL_version:',PChar(SSL_get_version(FSSL)),' SSL_cipher:',PChar(SSL_get_cipher_name(FSSL))]);
  end;
 end;

 function TClientData.session_connect:integer;
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
  ALPN;
  Case http2 of
   true :NewSession2;
   false:NewSession1;
  end;
  submit_path;
  session_recv;
  session_send;
  //bufferevent_close(bev:Pbufferevent)
 end;

 procedure TClientData.NewSession1;
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

 procedure TClientData.NewSession2;
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

 procedure TClientData.submit_request(nva:Pnghttp2_nv;nvlen:size_t;S:TStream);
 Var
  data_prd:Tnghttp2_data_provider;
  m:integer;
 begin
  if Assigned(S) then
  begin
   data_prd.source.ptr:=Pointer(S);
   //data_prd.read_callback:=@read_callback;
  end else
  begin
   data_prd:=Default(Tnghttp2_data_provider);
  end;

  if http2 then
  begin
   m:=nghttp2_submit_request(session,nil,nva,nvlen,@data_prd,nil);
  end else
  begin
   m:=fphttp1_submit_request(session,nil,nva,nvlen,@data_prd,nil);
  end;

  if m<0 then
  begin
   Log(irc_log,1,['error nghttp2_submit_request',m]);
  end;
 end;

 procedure TClientData.submit_path;
 const
  M_scheme:array[boolean] of PChar=(Scheme_http,Scheme_https);
 Var
  i,ext_len,hdr_len:SizeUInt;
  hdrs:Pnghttp2_nv;
 begin
  ext_len:=Length(headers) div 2;
  hdr_len:=ext_len+4;

  hdrs:=AllocMem(SizeOf(Tnghttp2_nv)*hdr_len);

  hdrs[0]:=make_nv_nocopy_name(NGHTTP2_method,PChar(Method));
  hdrs[1]:=make_nv_nocopy(NGHTTP2_scheme,M_scheme[http2]);

  hdrs[2]:=make_nv_nocopy_name(NGHTTP2_authority,PChar(Host));
  hdrs[3]:=make_nv_nocopy_name(NGHTTP2_path     ,PChar(Path));

  if ext_len>0 then
   For i:=0 to ext_len-1 do
   begin
    hdrs[4+i]:=MAKE_NV(PChar(headers[i*2]),PChar(headers[i*2+1]));
   end;

  submit_request(hdrs,hdr_len,nil);
 end;

 procedure TClientData.on_headers(stream_data:phttp2_stream_data;Const name,value:RawByteString);
 begin
 end;

 procedure TClientData.on_end_stream(stream_data:phttp2_stream_data);
 begin
 end;

 function TClientData.session_send:integer;
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

 function TClientData.session_recv:integer;
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

 procedure TClientData.session_shutdown(how:Longint);
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
  ClientData:TClientData;
 begin
  ClientData:=TClientData(ctx);
  if ClientData=nil then Exit;

  if (events and (BEV_EVENT_EOF or BEV_EVENT_ERROR or BEV_EVENT_TIMEOUT))<>0 then
  begin
   Log(irc_log,1,'BEV_EVENT_ERROR');
   FreeAndNil(ClientData);
   Exit;
  end;

  if (events and BEV_EVENT_READING)<>0 then
  begin
   if ClientData.session_recv<0 then
   begin
    ClientData.Free;
   end;
  end;

  if (events and BEV_EVENT_WRITING)<>0 then
  begin
   if ClientData.session_send<0 then
   begin
    ClientData.session_shutdown(1);
   end;
  end;

  if (events and BEV_EVENT_CONNECTED)<>0 then
  begin
   Log(irc_log,1,['BEV_EVENT_CONNECTED:',bufferevent_get_fd(bev),':',GetThreadID]);
   if ClientData.session_connect<0 then
   begin
    //bufferevent_close(ClientData.bev);
    //ClientData.session_shutdown(2);
    ClientData.Free;
   end;
  end;

 end;

 function on_frame_recv_callback(session: pnghttp2_session;
              frame:pnghttp2_frame; user_data: Pointer): Integer; cdecl;
 Var
  ClientData:TClientData;
  stream_data:phttp2_stream_data;

 begin

  ClientData:=TClientData(user_data);

  //print_flags('frame_f:',frame^.hd.flags);

  case frame^.hd._type of
   NGHTTP2_DATA   :;
   NGHTTP2_HEADERS:
   begin
    //print_type('frame_recv:',frame^.hd._type);
    //print_state('fr:',TClientData(user_data).stream_get_state(frame^.hd.stream_id));
   end;
  end;


  if (frame^.hd.flags and NGHTTP2_FLAG_END_HEADERS)<>0 then
  begin
   Log(irc_log,0,['NGHTTP2_FLAG_END_HEADERS:',frame^.hd.stream_id]);
  end;

  if (frame^.hd.flags and NGHTTP2_FLAG_END_STREAM)<>0 then
  begin
   Log(irc_log,0,['NGHTTP2_FLAG_END_STREAM:',frame^.hd.stream_id]);

   stream_data:=ClientData.get_stream_user_data(frame^.hd.stream_id);
   ClientData.on_end_stream(stream_data);

   {stream_data:=SessionData.get_stream_user_data(frame^.hd.stream_id);
   if stream_data=nil then exit(0);
   Result:=on_request_recv(SessionData, stream_data);}
  end;


  exit(0);
 end;

 function on_stream_close_callback(session: pnghttp2_session;
   stream_id: int32; error_code: uint32; user_data: Pointer): Integer; cdecl;
 Var
  ClientData:TClientData;
  stream_data:phttp2_stream_data;

 begin

   //print_state('sc:',TClientData(user_data).stream_get_state(stream_id));

  Log(irc_log,0,['stream_close:',stream_id,':',error_code]);
  ClientData:=TClientData(user_data);

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
  ClientData:TClientData;
  stream_data:phttp2_stream_data;

 begin
  Result:=0;

  //Writeln('on_header_callback');
  ClientData:=TClientData(user_data);

  case frame^.hd._type of
   NGHTTP2_HEADERS:
   begin

    {case frame^.headers.cat of
     NGHTTP2_HCAT_REQUEST      :Writeln('headers.cat:HCAT_REQUEST      ');
     NGHTTP2_HCAT_RESPONSE     :Writeln('headers.cat:HCAT_RESPONSE     ');
     NGHTTP2_HCAT_PUSH_RESPONSE:Writeln('headers.cat:HCAT_PUSH_RESPONSE');
     NGHTTP2_HCAT_HEADERS      :Writeln('headers.cat:HCAT_HEADERS      ');
    end;}

    //if (frame^.headers.cat<>NGHTTP2_HCAT_RESPONSE) then Exit;

    stream_data:=ClientData.get_stream_user_data(frame^.hd.stream_id);

    //Writeln(GetStr(Pointer(name),namelen),' ',GetStr(Pointer(value),valuelen));

    ClientData.on_headers(stream_data,GetStr(Pointer(name),namelen),GetStr(Pointer(value),valuelen));

    if (stream_data<>nil) then
    begin

     Case LowerCase(GetStr(Pointer(name),namelen)) of
      {NGHTTP2_method:Case GetStr(Pointer(value),valuelen) of
                      'HEAD'   :stream_data^.request_type:=0;
                      'GET'    :stream_data^.request_type:=1;
                      'POST'   :stream_data^.request_type:=2;
                      'CONNECT':stream_data^.request_type:=3;
                      else
                             stream_data^.request_type:=-1;
                     end;}
      NGHTTP2_path:if (stream_data^.request_path=nil) then
                   begin
                    //stream_data^.request_path:=CopyPChar(Pointer(value),valuelen);
                   end;
     end;

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
  ClientData:TClientData;
  stream_data:phttp2_stream_data;

 begin

  Result:=0;
  ClientData:=TClientData(user_data);

  //Writeln('on_begin_headers_callback:',frame^.hd._type);
  //print_type('begin_header:',frame^.hd._type);


  //print_state('state:',SessionData.stream_get_state(frame^.hd.stream_id));

  //Writeln('type:',frame^.hd._type,' cat:',frame^.headers.cat);

  if (frame^.hd._type<>NGHTTP2_HEADERS) or
     (frame^.headers.cat<>NGHTTP2_HCAT_RESPONSE) then Exit(0);

  stream_data:=ClientData.create_stream_data(frame^.hd.stream_id);

  ClientData.set_stream_user_data(frame^.hd.stream_id,stream_data);

  Log(irc_log,0,['create_stream:',frame^.hd.stream_id]);

 end;

 function on_recv_data(session:Pnghttp2_session;
               flags:uint8; stream_id:int32; data:Puint8; len:size_t;
               user_data:pointer):integer;cdecl;
 Var
  ClientData:TClientData;
 begin
  Result:=len;

  ClientData:=TClientData(user_data);
  if Assigned(ClientData.Recvs) then
  begin
   ClientData.Recvs.Write(data^,len);
  end;

  //print_flags(IntToStr(len)+' data_f:',flags);
  //Writeln('[DATA]:',len);
  //Write(GetStr(PChar(data),len));
  //if (flags and NGHTTP2_FLAG_END_STREAM)<>0 then Writeln;
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

 procedure replyConnect(var ClientData:TClientData;Const Method,Path:RawByteString);
 Var
  URI:TURI;
  ctx:PSSL_CTX;
  port:Word;
  q:RawByteString;
 begin
  Init_Callbacks;

  URI:=parse_uri(Path);
  ctx:=nil;
  port:=URI.GetPort;
  Case LowerCase(URI.getProtocol()) of
   '',
   'http' :if port=0 then port:=80;
   'https':begin
            if port=0 then port:=443;
            ctx:=app_ctx;
           end;
  end;
  Log(irc_log,0,['CONNECT TO:',URI.GetHost+':'+URI.GetPath,':',port]);

   if not Assigned(ClientData) then
   begin
    ClientData:=TClientData.Create_hostname(ctx,AF_INET,PAnsiChar(URI.GetHost),port);
    ClientData.Method:=Method;
    ClientData.Host:=URI.GetHost;
    ClientData.Path:=URI.GetPath(true);
    if ClientData.Path='' then ClientData.Path:='\';
    q:=URI.getQuery(true);
    if q<>'' then
    begin
     ClientData.Path:=ClientData.Path+'?'+URI.getQuery(true);
    end;
   end else
   begin
    ClientData.Method:=Method;
    ClientData.Path:=URI.GetPath(true);
    if ClientData.Path='' then ClientData.Path:='\';
    q:=URI.getQuery(true);
    if q<>'' then
    begin
     ClientData.Path:=ClientData.Path+'?'+URI.getQuery(true);
    end;
    ClientData.submit_path;
    ClientData.session_send;
   end;

 end;

end.

