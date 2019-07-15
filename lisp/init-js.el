(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when
		  (string-equal "tsx" (file-name-extension buffer-file-name))
		;; (tide-setup)
		(prettier-js-mode)
		(flycheck-mode -1)
		(setq web-mode-enable-auto-quoting nil))))
  ;; enable typescript-tslint checker
  ;;(flycheck-add-mode 'typescript-tslint 'web-mode)
  )

(use-package js2-mode
  :defer 1
  :config
  (add-hook 'js-mode-hook 'prettier-js-mode)
  (add-hook 'js-mode-hook 'add-node-modules-path)
  (setq js2-strict-missing-semi-warning nil
	js-indent-level 2
	css-indent-offset 2))

;; (use-package tide
;;   :defer 1
;;   :mode(("\\.ts\\'" . typescript-mode))
;;   :config
;;   (setq tide-disable-suggestions t)
;;   (add-hook 'typescript-mode-hook #'tide-setup)
;;   (add-hook 'typescript-mode-hook #'prettier-js-mode)
;;   (add-hook 'tide-mode-hook
;; 	    (lambda ()
;; 	      ;;(flycheck-mode 1)
;; 	      ;;(setq flycheck-check-syntax-automatically '(save mode-enabled))
;; 	      )))

(use-package prettier-js
  :config
  (setq prettier-js-args nil)
  (setq prettier-js-args '("--no-semi" "false"
                           "--jsx-single-quote" "true"
                           "--single-quote" "true")))

(use-package rjsx-mode
  :after prettier-js
  :mode(("\\.js\\'" . rjsx-mode)
	("\\.jsx\\'" . rjsx-mode))
  :init
  (add-hook 'rjsx-mode-hook 'prettier-js-mode)
  ;; (add-hook 'rjsx-mode-hook 'tide-setup)
  ;; (add-hook 'rjsx-mode-hook (lambda ()
  ;;                             (flycheck-mode -1)
  ;;                             (flycheck-select-checker 'javascript-eslint)
  ;;                             ))
  (add-hook 'flycheck-mode-hook 'lunaryorn-use-js-executables-from-node-modules)
  )

(defun camel-to-kebab (str)
  (let ((case-fold-search nil))
    (apply 'concat (cl-loop for c across str
			    if (char-equal c (upcase c))
			    collect (concat "-" (string (downcase c)))
			    else collect (string c)))))

(defun json-to-args (json)
  (mapcar
   (lambda (obj)
     (list
      (concat "--" (camel-to-kebab (symbol-name (car obj))))
      (if (eq t (cdr obj))
	  "true"
	(if (numberp (cdr obj))
	    (number-to-string (cdr obj))
	  (cdr obj)))))
   json))

(defun zac/new-component (name)
  (interactive "sNew component: ")
  
  ;; Component file
  (find-file (concat name ".js"))
  (insert (format "import React from 'react'
import PropTypes from 'prop-types'
import './styles.scss'

function %s(props) {
  return \"\"
}

%s.defaultProps = {

}

%s.propTypes = {

}

export default %s" name name name name))
  (goto-char (point-min))
  (save-buffer)

  ;; Spec file
  (find-file (concat name ".spec.js"))
  (insert (format "import React from 'react'
import { shallow } from 'enzyme'
import %s from './%s'

describe('%s', () => {
  let subject, props, Component

  const shallowed = () => {
    if (!subject) {
      subject = shallow(<Component {...props} />)
    }
    return subject
  }

  beforeEach(() => {
    Component = %s
    subject = null
    props = {
      key: 'key',
      name: 'name',
      disabled: true,
      checked: true,
      onChange: jest.fn()
    }
  })

  it('', () => {})  

})" name name name name name))

  ;; Story file
  (find-file (concat name ".story.js"))
  (insert (format "import React from 'react'
import { storiesOf } from '@storybook/react'
import { withKnobs } from '@storybook/addon-knobs'
import %s from './%s'

storiesOf('RBAC|%s', module)
  .addDecorator(withKnobs)
  .add('default', () => (
    <%s />
  ))
" name name name name))
  )


(defun lunaryorn-use-js-executables-from-node-modules ()
  "Set executables of JS checkers from local node modules."
  (-when-let* ((file-name (buffer-file-name))
               (root (locate-dominating-file file-name "node_modules"))
               (module-directory (expand-file-name "node_modules" root)))
    (pcase-dolist (`(,checker . ,module) '((javascript-jshint . "jshint")
                                           (javascript-eslint . "eslint")
                                           (javascript-jscs   . "jscs")))
      (let ((package-directory (expand-file-name module module-directory))
            (executable-var (flycheck-checker-executable-variable checker)))
        (when (file-directory-p package-directory)
          (set (make-local-variable executable-var)
               (expand-file-name (concat "bin/" module ".js")
                                 package-directory)))))))

(use-package mocha
  :ensure t
  :config (setq mocha-project-test-directory "app/assets/javascripts/test"))

(defun zac/create-action (name)
  (interactive "MAction name: ")
  (let ((name (upcase name)))
    (insert (format "export const %s = 'rbac/%s'" name name))))

(provide 'init-js)
