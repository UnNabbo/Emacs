(defun fixed-native--compile-async-skip-p
        (native--compile-async-skip-p file load selector)
    (let* ((naive-elc-file (file-name-with-extension file "elc"))
           (elc-file       (replace-regexp-in-string
                               "\\.el\\.elc$" ".elc" naive-elc-file)))
        (or (gethash elc-file comp--no-native-compile)
            (funcall native--compile-async-skip-p file load selector))))

(advice-add 'native--compile-async-skip-p
    :around 'fixed-native--compile-async-skip-p)

