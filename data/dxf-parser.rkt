;Dxf文件解析器

#lang racket

(require "structs.rkt")

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
      ;0-9，字符串（随着从 AutoCAD 2000 起引入了扩展符号名称，字数限制已由 255 个字符扩大到 2049 个单字节字符，不包括行末的换行符）：
      [(and (>= v 0) (<= v 9)) (cdr code)]
      ;10-39，双精度三维点值：
      [(and (>= v 10) (<= v 39)) (string->number (cdr code))]
      ;40-59，双精度浮点值：
      [(and (>= v 40) (<= v 59)) (string->number (cdr code))]
      ;60-79，16位整数值：
      [(and (>= v 60) (<= v 79)) (string->number (cdr code))]
      ;90-99，32位整数值：
      [(and (>= v 90) (<= v 99)) (string->number (cdr code))]
      ;100，字符串（最多 255 个字符；对于 Unicode 字符串，字符数要少一些）：
      [(= v 100) (cdr code)]
      ;102，字符串（最多 255 个字符；对于 Unicode 字符串，字符数要少一些）：
      [(= v 102) (cdr code)]
      ;105，表示16进制(hex)句柄值的字符串：????????????????????
      [(= v 105) (cdr code)]
      ;110-119，双精度浮点值：
      [(and (>= v 110) (<= v 119)) (string->number (cdr code))]
      ;120-129，双精度浮点值：
      [(and (>= v 120) (<= v 129)) (string->number (cdr code))]
      ;130-139，双精度浮点值：
      [(and (>= v 130) (<= v 139)) (string->number (cdr code))]
      ;140-149，双精度标量浮点值：
      [(and (>= v 140) (<= v 149)) (string->number (cdr code))]
      ;160-169，64位整数值：
      [(and (>= v 160) (<= v 169)) (string->number (cdr code))]
      ;170-179，16位整数值：
      [(and (>= v 170) (<= v 179)) (string->number (cdr code))]
      ;210-239，双精度浮点值：
      [(and (>= v 210) (<= v 239)) (string->number (cdr code))]
      ;270-279，16 位整数值：
      [(and (>= v 270) (<= v 279)) (string->number (cdr code))]
      ;280-289，16 位整数值：
      [(and (>= v 280) (<= v 289)) (string->number (cdr code))]
      ;290-299，布尔标志值：
      [(and (>= v 290) (<= v 299))
       (if (equal? "0" (cdr code)) #f #t)]
      ;300-309，任意字符串：
      [(and (>= v 300) (<= v 309)) (cdr code)]
      ;310-319，表示二进制数据块的十六进制值的字符串：
      [(and (>= v 310) (<= v 319)) (cdr code)]
      ;320-329，表示16进制句柄值的字符串：?????????????????
      [(and (>= v 320) (<= v 329)) (cdr code)]
      ;330-369，表示十六进制对象ID的字符串：?????????????????
      [(and (>= v 330) (<= v 369)) (cdr code)]
      ;370-379，16 位整数值：
      [(and (>= v 370) (<= v 379)) (string->number (cdr code))]
      ;380-389，16 位整数值：
      [(and (>= v 380) (<= v 389)) (string->number (cdr code))]
      ;390-399，表示 16 进制句柄值的字符串：
      [(and (>= v 380) (<= v 389)) (cdr code)]
      ;400-409，16 位整数值：
      [(and (>= v 380) (<= v 389)) (string->number (cdr code))]
      ;410-419，字符串：
      [(and (>= v 410) (<= v 419)) (cdr code)]
      ;420-429，32 位整数值：
      [(and (>= v 420) (<= v 429)) (string->number (cdr code))]
      ;430-439，字符串：
      [(and (>= v 430) (<= v 439)) (cdr code)]
      ;440-449，32 位整数值：
      [(and (>= v 440) (<= v 449)) (string->number (cdr code))]
      ;450-459，长整数：
      [(and (>= v 450) (<= v 459)) (string->number (cdr code))]
      ;460-469，双精度浮点值：
      [(and (>= v 460) (<= v 469)) (string->number (cdr code))]
      ;470-479，字符串：
      [(and (>= v 470) (<= v 479)) (cdr code)]
      ;480-481，表示 16 进制句柄值的字符串：
      [(and (>= v 480) (<= v 481)) (cdr code)]
      ;999，注释（字符串）：
      [(= v 999) (cdr code)]
      ;1000-1009，字符串（与 0-9 代码范围的限制相同）：
      [(and (>= v 1000) (<= v 1009)) (cdr code)]
      ;1010-1059，双精度浮点值：
      [(and (>= v 1010) (<= v 1059)) (string->number (cdr code))]
      ;1060-1070，16 位整数值：
      [(and (>= v 1060) (<= v 1070)) (string->number (cdr code))]
      ;1071，32 位整数值：
      [(= v 1071) (string->number (cdr code))])))

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
  (parse-header in)
  ;类组码("CLASSES")：
  (parse-classes in)
  ;符号表组码("TABLES")：
  ;(parse-tables in)
  ;块组码("BLOCKS")：
  ;(parse-blocks in)
  ;图元组码("ENTITIES")：
  ;(parse-entities in)
  ;对象组码("OBJECTS")：
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