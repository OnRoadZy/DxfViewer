;Dxf文件解析器

#lang racket

(define file-name "..//test//sample(2013)")
(define file-suffix ".dxf")
(define path (string-append file-name file-suffix))

;通用函数==========================================
;关联值对：
;从DXF文件中逐行读取值，组成关联值对供解析使用。
;(-> input-port? pair?)
(define (value-pair in)
  (cons
   (string-trim (read-line in))
   (string-trim (read-line in))))

;解析组码值：
(define (parse-value code)
  (let ([v (string->number (car code))])
    (cond
      
      [(and
        ;0-9，字符串（随着从 AutoCAD 2000 起引入了扩展符号名称，字数限制已由 255 个字符扩大到 2049 个单字节字符，不包括行末的换行符）：
        (>= v 0) (<= v 9)
        ;100，字符串（最多 255 个字符；对于 Unicode 字符串，字符数要少一些）：
        ;102，字符串（最多 255 个字符；对于 Unicode 字符串，字符数要少一些）：
        ;105，表示16进制(hex)句柄值的字符串：
        ;999，注释（字符串）：
        ;1071，32 位整数值：
        (= v 100 102 105 999 1071)
        ;300-309，任意字符串：
        (>= v 300) (<= v 309)
        ;310-319，表示二进制数据块的十六进制值的字符串：
        (>= v 310) (<= v 319)
        ;320-329，表示16进制句柄值的字符串：
        (>= v 320) (<= v 329)
        ;330-369，表示十六进制对象ID的字符串：
        (>= v 330) (<= v 369)
        ;390-399，表示 16 进制句柄值的字符串：
        (>= v 380) (<= v 389)
        ;410-419，字符串：
        (>= v 410) (<= v 419)
        ;430-439，字符串：
        (>= v 430) (<= v 439)
        ;470-479，字符串：
        (>= v 470) (<= v 479)
        ;480-481，表示 16 进制句柄值的字符串：
        (>= v 480) (<= v 481)
        ;1000-1009，字符串（与 0-9 代码范围的限制相同）：
        (>= v 1000) (<= v 1009))
       (cdr code)]
      [(and
        ;10-39，双精度三维点值：
        (>= v 10) (<= v 39)
            ;40-59，双精度浮点值：
            (>= v 40) (<= v 59)
            ;60-79，16位整数值：
            (>= v 60) (<= v 79)
            ;90-99，32位整数值：
            (>= v 90) (<= v 99)
            ;110-119，双精度浮点值：
            (>= v 110) (<= v 119)
            ;120-129，双精度浮点值：
            (>= v 120) (<= v 129)
            ;130-139，双精度浮点值：
            (>= v 130) (<= v 139)
            ;140-149，双精度标量浮点值：
            (>= v 140) (<= v 149)
            ;160-169，64位整数值：
            (>= v 160) (<= v 169)
            ;170-179，16位整数值：
            (>= v 170) (<= v 179)
            ;210-239，双精度浮点值：
            (>= v 210) (<= v 239)
            ;270-279，16 位整数值：
            (>= v 270) (<= v 279)
            ;280-289，16 位整数值：
            (>= v 280) (<= v 289)
            ;370-379，16 位整数值：
            (>= v 370) (<= v 379)
            ;380-389，16 位整数值：
            (>= v 380) (<= v 389)
            ;400-409，16 位整数值：
            (>= v 380) (<= v 389)
            ;420-429，32 位整数值：
            (>= v 420) (<= v 429)
            ;440-449，32 位整数值：
            (>= v 440) (<= v 449)
            ;450-459，长整数：
            (>= v 450) (<= v 459)
            ;460-469，双精度浮点值：
            (>= v 460) (<= v 469)
            ;1010-1059，双精度浮点值：
            (>= v 1010) (<= v 1059)
            ;1060-1070，16 位整数值：
            (>= v 1060) (<= v 1070))
       (string->number (cdr code))]
      ;290-299，布尔标志值：
      [(and (>= v 290) (<= v 299))
       (if (equal? "0" (cdr code)) #f #t)])))
      
;定义二维点和三位点结构：
(struct 2dp (x y))
(struct 3dp (x y z))

;解析二维点：
(define  (parse-2dp in)
  (let* ([1p (value-pair in)]
         [2p (value-pair in)]
         [1pc (string->number (car 1p))]
         [2pc (string->number (car 2p))])
    (when (and (>= 1pc 10) (<= 1pc 39)
               (>= 2pc 10) (<= 2pc 39))
        (2dp (parse-value 1p) (parse-value 2p)))))

;解析三维点：
(define  (parse-3dp in)
  (let* ([1p (value-pair in)]
         [2p (value-pair in)]
         [3p (value-pair in)]
         [1pc (string->number (car 1p))]
         [2pc (string->number (car 2p))]
         [3pc (string->number (car 3p))])
    (when (and (>= 1pc 10) (<= 1pc 39)
               (>= 2pc 10) (<= 2pc 39)
               (>= 3pc 10) (<= 3pc 39))
        (3dp (parse-value 1p) (parse-value 2p) (parse-value 3p)))))

;解析DXF文件=======================================
;读入DXF文件：
(define (load-dxf file)
  (call-with-input-file file
    (lambda (in)
      (parse-dxf in))))

;测试解析结果：----------------
(module+ test
  (load-dxf path)
  ;显示所有系统变量：
  system-var
  ;显示所有类：
  classes)

;解析DXF文件内容：
(define (parse-dxf in)
  (let ([code (value-pair in)])
    (when (and (equal? (car code) "0")
               (equal? (cdr code) "SECTION"))
      (parse-section in))))

;解析SECTION段：
(define (parse-section in)
  ;标题组码("HEADER")：
  (print "解析HEADER段……")
  (let ([code (value-pair in)])
    (when (and (equal? (car code) "2")
               (equal? (cdr code) "HEADER"))
      (parse-header in)))
  ;类组码("CLASSES")：
  (print "解析CLASSES段……")
  (let ([code (value-pair in)])
    (when (and (equal? (car code) "2")
               (equal? (cdr code) "CLASSES"))
      (parse-classes in)))
  ;符号表组码("TABLES")：
  (print "解析TABLES段……")
  (let ([code (value-pair in)])
    (when (and (equal? (car code) "2")
               (equal? (cdr code) "TABLES"))
      (parse-tables in)))
  ;块组码("BLOCKS")：
  ;(print "解析BLOCKS段……")
  ;(parse-blocks in)
  ;图元组码("ENTITIES")：
  ;(print "解析ENTITIES段……")
  ;(parse-entities in)
  ;对象组码("OBJECTS")：
  ;(print "解析OBJECTS段……")
  ;(parse-objects in)
  )

;;解析标题组码（HEADER）：------------------------------------------
;定义系统变量散列表：
(define system-var (make-hash))

;解析标题组码（HEADER）：
(define (parse-header in)
  (let ([code (value-pair in)])
    (when (and (equal? (car code) "2")
               (equal? (cdr code) "HEADER"))
      (parse-system-val in))))

;解析系统变量：
(define (parse-system-val in)
  (do ([code (value-pair in) (value-pair in)])
    ((and (string=? (car code) "0")
          (string=? (cdr code) "ENDSEC")) void)
    (when (equal? (car code) "9")
        (case (cdr code)
          [("$LIMMIN" "$LIMMAX"
            "$PLIMMIN" "$PLIMMAX")
           (hash-set! system-var
                      (cdr code)
                      (parse-2dp in))]
          [("$INSBASE"
            "$EXTMIN" "$EXTMAX"
            "$UCSORG" "$UCSXDIR" "$UCSYDIR"
            "$UCSORGTOP" "$UCSORGBOTTOM" "$UCSORGLEFT" "$UCSORGRIGHT"
            "$UCSORGFRONT" "$UCSORGBACK"
            "$PUCSORG" "$PUCSXDIR" "$PUCSYDIR"
            "$PUCSORGTOP" "$PUCSORGBOTTOM" "$PUCSORGLEFT" "$PUCSORGRIGHT"
            "$PUCSORGFRONT" "$PUCSORGBACK"
            "$PINSBASE"
            "$PEXTMIN" "$PEXTMAX")
           (hash-set! system-var
                      (cdr code)
                      (parse-3dp in))]
          [else
           (hash-set! system-var
                      (cdr code)
                      (parse-value (value-pair in)))]))))

;解析类组码（CLASSES）：-------------------------------------------------
;定义类结构：
(struct class (c1 c2 c3 c90 c280 c281))

;定义类列表：
(define classes '())

;解析类组码（CLASSES）：
(define (parse-classes in)
  (do ([code (value-pair in) (value-pair in)])
    ((and (equal? (car code) "0")
          (equal? (cdr code) "ENDSEC")) void)
    (when (and (equal? (car code) "0")
               (equal? (cdr code) "CLASS"))
      (parse-class in))))

;解析类：
(define (parse-class in)
  (let ([c1 (value-pair in)]
        [c2 (value-pair in)]
        [c3 (value-pair in)]
        [c90 (value-pair in)]
        [c280 (value-pair in)]
        [c281 (value-pair in)])
    (set! classes
          (cons
           (class (cdr c1) (cdr c2) (cdr c3)
             (cdr c90) (cdr c280) (cdr c281))
           classes))))

;解析表组码（TABLES）：-------------------------------------------------
;定义表列表：
(define tables '())

;定义符号表条目（TABLE）结构：
(struct table (t2 t5/105 t100 t70 entries))

;定义条目数据结构：
(struct entry (e5 e100 datas))

;解析表组码：
(define (parse-tables in)
  (do ([code (value-pair in) (value-pair in)])
    ((and (equal? (car code) "0")
          (equal? (cdr code) "ENDSEC")) void)
    (when (and (equal? (car code) "0")
               (equal? (cdr code) "TABLE"))
      (set! tables
            (cons (parse-table in) tables)))))

;解析表：
(define (parse-table in)
  (let ([t2 (value-pair in)]
        [t5/105 (value-pair in)]
        [t100 (value-pair in)]
        [t70 (value-pair in)])
    (table (cdr t2) (cdr t5/105)
           (cdr t100) (cdr t70)
           (parse-entries in))))

;解析条目：
(define (parse-entries in)
  ;APPID符号表条目：
  (let ([code (value-pair in)])
    (when (and (equal? (car code) "0")
               (equal? (cdr code) "APPID"))
      (parse-appid in)))
  ;(parse-dimstyle in)
  ;(parse-layer in)
  ;(parse-ltype in)
  ;(parse-style in)
  ;(parse-ucs in)
  ;(parse-view in)
  ;(parse-vport in)
  ;(parse-block_record in)
  )

;解析APPID符号表条目：
(define (parse-appid in)
  void)

;-----------------------------------------------
#|
;取得图元并分类获取数据：
(define (get-entities in)
  (let ([code0 (value-pair in)])
    (do ([code code0 (value-pair in)])
      ((string=? (cdr code) "ENDSEC") 'ENDSEC)
      (when (equal? (car code) "0")
          (case (cdr code)
            [("LINE") ;线段
             (get-line in)]
            [("CIRCLE") ;圆
             (get-circle in)]
            [else '未知的实体])))))

(struct line (height p-start p-end p-stretch))
|#

#|
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
|#

#|
;取得point的值：
(define (get-point code in)
  (let ([x (get-x/y/z "10" code in)]
        [y (get-x/y/z "20" code in)]
        [z (get-x/y/z "20" code in)])
    (point x y z)))

;根据给定的组码，取得x或y或z的值：
(define (get-x/y/z code1 code in)
  (let ([code1 (read-code in)])
    (when (equal? code1 (car code))
      (string->number (cdr code)))))
|#