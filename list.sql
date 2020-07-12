
//все записи
//SELECT * FROM story;

//последние 300 записей
SELECT * FROM story LIMIT 300 OFFSET (SELECT COUNT(*) FROM story)-300;

