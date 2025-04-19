
(defun my/with-foreground (face str)
  (declare (indent 1))
  (add-face-text-property 0 (length str)
                          `(:foreground ,(face-foreground face)) nil str)
  str)

(defun my/with-background (face str)
  (declare (indent 1))
  (add-face-text-property 0 (length str)
                          `(:background ,(face-background face)) nil str)
  str)

(defun my/with-bold (str)
  (declare (indent 1))
  (add-face-text-property 0 (length str)
                          `(:weight bold) nil str)
  str)

(provide 'my-text-property-helpers)
