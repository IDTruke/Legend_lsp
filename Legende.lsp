;*******************************************************************;
;***                                                             ***;
;***                      Générateur de légende                  ***;
;***                                                             ***;
;*******************************************************************;

;***Utilisation: saisir "legende" dans Autocad
(defun c:legende (/ calqueLGD oldStyle lstObj nbObj multiSel ins)

  ;*********************GESTION DES CALQUES**********************;
  (setq calqueLGD "00_Legende")
  (if (not (tblsearch "LAYER" calqueLGD)) (command "._layer" "_M" calqueLGD "_C" 250 "" ""))
  (activeLayer calqueLGD)

  ;*****************GESTION DU STYLE DE TEXTE********************;
  (setq oldstyle (getvar "textstyle"))
  (command "._Style" "PTT_lsp" "arial.ttf" 0.4 1 0 "N" "N")

  ;*****************GESTION DU TYPE DE LIGNE*********************;
  (command "_.LINETYPE" "A" "Continuous" "" "")

  ;**************Ajout des objets à la légende********************;
  ;*******************Choix du type d'objet***********************;
  (setq lstObj '())
  (setq lstObj (list (selectObj 0)))
  (setq nbObj 1)
  (setq multiSel "Oui")
  (while (= multiSel "Oui")
    (initget 1 "Oui Non")
    (setq multiSel (getkword "\nSélection supplémentaire ? [Oui/Non] :"))
    (if (= multiSel "Oui")
      (and (setq lstObj (append lstObj (list (selectObj 0)))) (setq nbObj (+ nbObj 1)))
      (alert "Fin de la sélection") ) )

  ;**************Choix de la zone de tracage********************;
  (setq ins (getpoint "\nChoisir le point d'insertion: "))

  ;***********Dessine les éléments de la légende****************;
  (dessineLGD ins (+ nbObj 1) (triLGD lstObj))
  (debugMsg "Information" "Traitement terminé")
  (princ) )


;***************************************************************;
;*********************FONCTIONS EXTERNES************************;
;***************************************************************;

;*********************GESTION DES CALQUES**********************;
(defun activeLayer (layer)
  (command "._layer" "_S" layer "")
  (princ) )

;*********************Sélection des objets**********************;
(defun selectObj (x)
  (setq selObj (Selection 0))
  (cond
    ((= "LWPOLYLINE" (value selObj 0 "Empty")) (setq lst (list "L" (getstring 1 "\nTexte de légende") selObj)))
    ((= "HATCH" (value selObj 0 "Empty")) (setq lst (list "H" (getstring 1 "\nTexte de légende") selObj)))
    (T (DebugMsg "Erreur" "Type d'objet inconnu!")) )
  lst )

;**************************Sélection****************************;
(defun Selection (x)
  (setq linSel (entsel))
  (setq linInfo (entget (car linSel)))
  (setq result (append (list (cadr linInfo)) (cdddr linInfo))) ;élimine les informations problématique avec < ou >
  result )

;*********************Dessine la légende***********************;
(defun dessineLGD (ptInsert nbLines lst)
  (setq x (car ptInsert))
  (setq y (cadr ptInsert))
  (setq larg 20)
  (setq htr (+ 2 (* nbLines 3)))
  (drawObj "L" (list (list x y) (list (+ x larg) y) (list (+ x larg) (+ y htr)) (list x (+ y htr))) 250 1 "Continuous" 0.0 0.0 1)
  (M-Text (list (+ x (/ larg 2)) (+ y (- htr 1))) "Légende" 5)
  (setq n nbLines)
  (foreach elem lst
    (if (= "L" (car elem))
      (drawObj
	(car elem)
        (list
	  (list (+ x 2) (+ y (* (- n 1) 3)))
	  (list (+ x 5) (+ y (* (- n 1) 3))) )
        (value (caddr elem) 62 (layerColor (caddr elem)))
	0
        (value (caddr elem) 6 "Continuous")
        (value (caddr elem) 40 0.0)
        (value (caddr elem) 41 0.0)
	1 )
      (drawObj
	(car elem)
	(list
	  (list (+ x 2) (+ (+ y (* (- n 1) 3)) 1))
	  (list (+ x 5) (+ (+ y (* (- n 1) 3)) 1))
	  (list (+ x 5) (- (+ y (* (- n 1) 3)) 1))
	  (list (+ x 2) (- (+ y (* (- n 1) 3)) 1)) )
	(value (caddr elem) 62 (layerColor (caddr elem)))
	(value (caddr elem) 52 0.0)
	(value (caddr elem) 2 "ANSI32")
	0
	0
	(value (caddr elem) 41 0.05) ) )
    (M-Text (list (+ x 8) (+ y (* (- n 1) 3))) (cadr elem) 4)
    (setq n (- n 1)) )
  (princ) )

;****************Recuperation couleur calque******************;
(defun layerColor (elem)
  (setq lyr (cdr (assoc 8 elem)))
  (setq result (cdr (assoc 62 (tblsearch "LAYER" lyr))))
  result )

;********************Retour valeur data***********************;
(defun value (elem cle def)
  (setq ret (assoc cle elem))
  (if (car ret)
    (setq result (cdr ret))
    (setq result def) )
  result )

;********************Tri infos legende***********************;
(defun triLGD (lst)
  (setq lstL '())
  (setq lstH '())
  (foreach elem lst
    (cond
      ((= "L" (car elem)) (setq lstL (append lstL (list elem))))
      ((= "H" (car elem)) (setq lstH (append lstH (list elem)))) ) )
  (setq result (append lstL lstH))
  result )

;*******************Création polyligne************************;
;+ d'infos : https://www.autodesk.com/techpubs/autocad/acad2000/dxf/lwpolyline_dxf_06.htm
(defun drawObj (typ lst color cls linetype startW endW scale)
  (cond
    ((= typ "L")
     (setq tmp '())
     (entmakex (append (list (cons 0 "LWPOLYLINE")
			     (cons 100 "AcDbEntity")
                             (cons 100 "AcDbPolyline")
                             (cons 90 (length lst))
                             (cons 70 cls)
			     (cons 62 color)
			     (cons 6 linetype) )
		        (foreach elem (reverse (mapcar (function (lambda (p) (cons (cons 10 p) (list (cons 40 startW) (cons 41 endW))))) lst))
			  (setq tmp (append tmp elem)) ) )) )
    ((= typ "H")
     (entmakex-hatch (list lst) cls linetype scale color) ) ) )

;*********************Création texte**************************;
;+ d'infos : https://www.autodesk.com/techpubs/autocad/acad2000/dxf/mtext_dxf_06.htm
(defun M-Text (pt str align)
  (entmakex (list (cons 0 "MTEXT")
		  (cons 100 "AcDbEntity")
		  (cons 100 "AcDbMText")
		  (cons 40 1)
		  (cons 71 align) ;1 = Top left, 2 = Top center, 3 = Top right, 4 = Middle left, 5 = Middle center, 6 = Middle right, 7 = Bottom left, 8 = Bottom center, 9 = Bottom right
		  (cons 10 pt)
		  (cons 1 str) )))

;************************Déboguage****************************;
(defun debugMsg (txt var)
  (print (strcat txt " : "))(prin1 var) )

;*********************Création hachures***********************;
;; By ElpanovEvgeniy
(defun entmakex-hatch (L a n s c)
 ;; L - list point
 ;; A - angle hatch
 ;; N - name pattern
 ;; S - scale
 ;; C - color

 ;; returne - hatch ename
 (entmakex
  (apply
   'append
   (list
    (list '(0 . "HATCH")
          '(100 . "AcDbEntity")
          '(410 . "Model")
          '(100 . "AcDbHatch")
	  (cons 62 c)
          '(10 0.0 0.0 0.0)
          '(210 0.0 0.0 1.0)
          (cons 2 n)
          (if (= n "_SOLID")
           '(70 . 1)
           '(70 . 0)
          ) ;_  if
          '(71 . 0)
          (cons 91 (length L))
    ) ;_  list
    (apply 'append
           (mapcar '(lambda (a)
                     (apply 'append
                            (list (list '(92 . 7) '(72 . 0) '(73 . 1) (cons 93 (length a)))
                                  (mapcar '(lambda (b) (cons 10 b)) a)
                                  '((97 . 0))
                            ) ;_  list
                     ) ;_  apply
                    ) ;_  lambda
                   l
           ) ;_  mapcar
    ) ;_  apply
    (list '(75 . 0)
          '(76 . 1)
          (cons 52 a)
          (cons 41 s)
          '(77 . 0)
          '(78 . 1)
          (cons 53 a)
          '(43 . 0.)
          '(44 . 0.)
          '(45 . 1.)
          '(46 . 1.)
          '(79 . 0)
          '(47 . 1.)
          '(98 . 2)
          '(10 0. 0. 0.0)
          '(10 0. 0. 0.0)
          '(451 . 0)
          '(460 . 0.0)
          '(461 . 0.0)
          '(452 . 1)
          '(462 . 1.0)
          '(453 . 2)
          '(463 . 0.0)
          '(463 . 1.0)
          '(470 . "LINEAR")
    ) ;_  list
   ) ;_  list
  ) ;_  apply
 ) ;_  entmakex
) ;_  defun


