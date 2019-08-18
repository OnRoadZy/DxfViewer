;Dxf文件解析器

#lang racket

(define file-name "sample(2013)")
(define file-suffix ".dxf")
(define path (string-append file-name file-suffix))

;读入DXF文件：
(define (load-dxf file)
  (call-with-input-file file
    (lambda (in)
      (show-dxf in))))

;显示DXF文件内容：
(define (show-dxf in)
  (let ([code0 (read-code in)])
    (do ([code code0 (read-code in)])
      ((string=? (cdr code) "EOF") 'EOF)
      (unless (and (equal? (car code) "0")
                 (equal? (cdr code) "SECTION"))
        (get-element (read-code in) in)))))

;对组码进行分类处理：
(define (get-element code in)
  (when (equal? (car code) "2")
      (case (cdr code)
        [("HEADER") '标题组码]
        [("CLASSES") '类组码]
        [("TABLES") '符号表组码]
        [("BLOCKS") '块组码]
        [("ENTITIES") ;图元组码
         (get-entities in)]
        [("OBJECTS") '对象组码]
        [else '缺乏对应的标志字符串])))

;取得图元并分类获取数据：
(define (get-entities in)
  (let ([code0 (read-code in)])
    (do ([code code0 (read-code in)])
      ((string=? (cdr code) "ENDSEC") 'ENDSEC)
      (when (equal? (car code) "0")
          (case (cdr code)
            [("LINE") ;线段
             (get-line in)]
            [("CIRCLE") ;圆
             (get-circle in)]
            [else '未知的实体])))))

(struct line (height p-start p-end p-stretch))

;供测试用：
(define (load-dxf file)
  (call-with-input-file "line.dxf"
    (lambda (in)
      (get-line (read-code in)
                in))))

;获取line线段数据：
(define (get-line in)
  (let ([code0 (read-code in)])
  (do ([code code0 (read-code in)])
    ((equal? (car code) "0") code)
    (cond (car code);?????????????????????????????????????????????
      [("10" "20" "30") ([p-start (get-point code in)]
            [p-end (get-point code in)]
            [p-stretch (get-point code in)])
        (line height p-start p-end p-stretch)))))

;取得point的值：
(define (get-point code in)
  (let ([x (get-x/y/z "10" code in)]
        [y (get-x/y/z "20" code in)]
        [z (get-x/y/z "20" code in)])
    (point x y z)))

;读取组码（代码/值对）：
(define (read-code in)
  (cons
   (string-trim (read-line in))
   (string-trim (read-line in))))

;根据给定的组码，取得x或y或z的值：
(define (get-x/y/z code1 code in)
  (let ([code1 (read-code in)])
    (when (equal? code1 (car code))
      (string->number (cdr code)))))