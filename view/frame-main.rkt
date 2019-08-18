#lang racket

(require racket/gui)

(provide main-frame)

;定义主界面=======================
(define frame-current-width 800)
(define frame-current-height 600)

(define main-frame
  (new frame%
       [label "DXF浏览器——DxfViewer"]
       [width frame-current-width]
       [height frame-current-width]
       [border 5]))

;定义菜单：==================================
(define menu-bar
  (new menu-bar%
       [parent main-frame]))

;定义菜单定义宏：------------------------------------------
;主菜单：
(define-syntax-rule (main-menu c)
  (new menu%
       [parent menu-bar]
       [label c]))

;子菜单：
(define-syntax-rule (sub-menu p c f)
  (new menu-item%
       [parent p]
       [label c]
       [callback (lambda (item event) f)]))

;定义菜单：-----------------------------------------------
;主菜单：
(define menu/file (main-menu "文件(&F)"))
(define menu/help (main-menu "帮助(&H)"))
;文件菜单：
(define menu/file/exit (sub-menu menu/file "退出(&X)" (app-exit)))
;帮助菜单：
(define menu/help/about (sub-menu menu/help "关于(&A)" (help-about)))

;菜单回调函数：============================================
;退出程序：
(define (app-exit)
  (send main-frame on-exit))

;关于：
(define (help-about)
  (message-box  "关于本程序" "本程序用来显示AutoCAD的dxf文件。"
   main-frame
   (list 'ok)))

;定义视图面板：================================================
;总容器：
(define pane/all
  (new vertical-pane%
       [parent main-frame]
       [alignment (list 'left 'top)]))

;工具栏：------------------------------------------------------
(define pane/toolbar
  (new horizontal-pane%
       [parent pane/all]
       [alignment (list 'left 'top)]
       [stretchable-height #f]))

;工具按钮通用宏：
(define-syntax-rule (toolbutton p c f)
  (new button%
       [parent p]
       [label c]
       [callback
        (lambda (item event) (f item event))]))

;定义按钮：
(define tb/open (toolbutton pane/toolbar "打开" void))
(define tb/about (toolbutton pane/toolbar "关于" (help-about)))
(define tb/exit (toolbutton pane/toolbar "退出" (app-exit)))

;视图区：--------------------------------------------------------
(define pane/view
  (new horizontal-pane%
       [parent pane/all]
       [alignment (list 'left 'bottom)]))

;画布：
(define canvas
  (new canvas%
       [parent pane/view]
       [paint-callback
        (lambda (canvas dc) void)]))

;状态栏：--------------------------------------------------------
(define pane/statusbar
  (new horizontal-pane%
       [parent pane/all]
       [alignment (list 'left 'bottom)]
       [stretchable-height #f]))

;信息栏：
(define status/message
  (new message%
       [parent pane/statusbar]
       [label "程序准备就绪！"]
       [auto-resize #t]))

