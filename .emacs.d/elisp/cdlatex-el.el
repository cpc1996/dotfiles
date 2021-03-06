(setq cdlatex-simplify-sub-super-scripts nil)
(setq cdlatex-math-modify-alist
      '((?B "\\mathbb" nil t nil nil)
        (?R "\\textrm" nil t nil nil)
        (?F "\\mathfrak" nil t nil nil)
        (?_ "_" nil t nil nil)
        (?^ "^" nil t nil nil)
        (?l "_" nil t nil nil)
        (?h "^" nil t nil nil)
        (?a "" nil t nil nil "(" ")") ; Needs custom cdlatex
        ))
(setq cdlatex-math-symbol-alist
      '((?B "\\mathbb{?}" "\\mathbf{?}")
        (?R "\\sqrt{?}" "\\sqrt[?]{}")
        (?I "\\implies")
        (?. "\\cdot" "\\circ" "\\dots")
        ;; (?: ";")
        ))
(setq cdlatex-command-alist
      ;; Assuming the matching pair is already inserted
      ;; TODO: Handle existing matching pair through hooks
      '(
        ;; ("p{" "Insert a \\{ \\} pair" "\\{?\\" cdlatex-position-cursor nil t t)
        ;; ("p(" "Insert a \\( \\) pair" "\\(?\\" cdlatex-position-cursor nil t t)
        ;; TODO: Figure out how to get this work
        ;; ("\\(" "Insert a \\( \\) pair" "\\(?\\" cdlatex-position-cursor nil t t)
        ;; ("\\{" "Insert a \\{ \\} pair" "\\{?\\" cdlatex-position-cursor nil t t)
        ;; ("l[" "Insert a \\[ ... \\] pair" "\\[ ? \\]" cdlatex-position-cursor nil t t)
        ("lp" "Insert a \\[ ... \\] pair" "\\[ ? \\]" cdlatex-position-cursor nil t t)
        ;; ("l{" "Insert a \\{ \\} pair" "\\{?\\" cdlatex-position-cursor nil t t)
        ("dim" "Insert \\dim" "\\dim" cdlatex-position-cursor nil t t)
        ("rank" "Insert \\rank" "\\rank" cdlatex-position-cursor nil t t)
        ("nullity" "Insert \\nullity" "\\nullity" cdlatex-position-cursor nil t t)
        ("le" "Insert \\le" "\\le" cdlatex-position-cursor nil t t)
        ("ge" "Insert \\ge" "\\ge" cdlatex-position-cursor nil t t)
        ("lpi" "Insert \\pi" "\\pi" cdlatex-position-cursor nil t t)
        ("lb" "Insert a \\{ \\} pair" "\\{?\\}" cdlatex-position-cursor nil t t)
        ("lB" "Insert a \\{ \\} pair" "\\{ ? \\}" cdlatex-position-cursor nil t t)
        ("l8" "Insert \\infty" "\\infty" cdlatex-position-cursor nil t t)
        ("-8" "Insert \\infty" "\\infty" cdlatex-position-cursor nil t t)
        ("int" "Insert \\int_{}^{}" "\\int_{?}^{}" cdlatex-position-cursor nil t t)
        ("int0" "Insert \\int" "\\int" cdlatex-position-cursor nil t t)
        ("in" "Insert \\in" "\\in" cdlatex-position-cursor nil t t)
        ("to" "Insert \\to" "\\to" cdlatex-position-cursor nil t t)
        ("pm" "Insert \\pm" "\\pm" cdlatex-position-cursor nil t t)
        ("mod" "Insert \\mod" "\\mod" cdlatex-position-cursor nil t t)
        ("gcd" "Insert \\gcd" "\\gcd" cdlatex-position-cursor nil t t)
        ("lcm" "Insert \\lcm" "\\lcm" cdlatex-position-cursor nil t t)
        ("df" "Insert \\dfrac{}{}" "\\dfrac{?}{}" cdlatex-position-cursor nil t t)
        ("tf" "Insert \\tfrac{}{}" "\\tfrac{?}{}" cdlatex-position-cursor nil t t)
        ("root" "Insert \\sqrt{}" "\\sqrt{?}" cdlatex-position-cursor nil t t)
        ("abs" "Insert \\abs{}" "\\abs{?}" cdlatex-position-cursor nil t t)
        ("norm" "Insert \\norm{}" "\\norm{?}" cdlatex-position-cursor nil t t)
        ("th" "Insert \\theta" "\\theta" cdlatex-position-cursor nil t t)
        ("sin" "Insert \\sin" "\\sin" cdlatex-position-cursor nil t t)
        ("cos" "Insert \\cos" "\\cos" cdlatex-position-cursor nil t t)
        ("tan" "Insert \\tan" "\\tan" cdlatex-position-cursor nil t t)
        ("sec" "Insert \\sec" "\\sec" cdlatex-position-cursor nil t t)
        ("asin" "Insert \\arcsin" "\\arcsin" cdlatex-position-cursor nil t t)
        ("acos" "Insert \\arccos" "\\arccos" cdlatex-position-cursor nil t t)
        ("atan" "Insert \\arctan" "\\arctan" cdlatex-position-cursor nil t t)
        ("rvert" "Insert \\rvert" "\\rvert" cdlatex-position-cursor nil t t)
        ("ln" "Insert \\ln" "\\ln" cdlatex-position-cursor nil t t)
        ("prodl" "Insert \\prod\\limits_{}^{}" "\\prod\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
        ("int" "Insert \\int_{}^{}" "\\int_{?}^{}" cdlatex-position-cursor nil nil t)
        ("liml" "Insert \\lim\\limits_{}" "\\lim\\limits_{?}" cdlatex-position-cursor nil nil t)
        ;; ("cen" "Insert an inline CENTER environment template" "\\begin{center} ? \\end{center}" cdlatex-position-cursor nil t t)
        ("ctr" "Insert a CENTER environment template" "" cdlatex-environment ("center") t nil) ; This is a bit buggy, and does not respect indentation
        ))
(setq cdlatex-math-symbol-prefix ?\;)
(add-hook 'org-cdlatex-mode-hook
          (lambda ()
            ;; (define-key org-cdlatex-mode-map (kbd "'") 'nil)
            (define-key org-cdlatex-mode-map (kbd ";") 'cdlatex-math-symbol)))
