(setq org-publish-project-alist
	  '(		
		("org-stathissideris"
		 ;; Path to your org files.
		 :base-directory "s:/global/blog/"
		 :base-extension "org"

		 ;; Path to your Jekyll project.
		 :publishing-directory "s:/virtual_machines/justine-shared-folder/site-test/"
		 :recursive t
		 :publishing-function org-publish-org-to-html
		 :headline-levels 4 
		 :html-extension "html"
		 :body-only t ;; Only export section between <body> </body>
		 )

		("org-static-stathissideris"
		 :base-directory "s:/global/blog/"
		 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php\\|pl\\|zip"
		 :publishing-directory "s:/virtual_machines/justine-shared-folder/site-test/"
		 :recursive t
		 :publishing-function org-publish-attachment)

		("blog" :components ("org-stathissideris" "org-static-stathissideris"))
		))

(defun youtube (id)
  (let ((width 400)
		(height 225))
	(concat
	 "<object width=\"" (number-to-string width) "\" height=\"" (number-to-string height) "\">"
	 "<param name=\"movie\" value=\"http://www.youtube.com/v/"
	 id
	 "&amp;hl=en_US&amp;fs=1?rel=0&amp;color1=0x5d1719&amp;color2=0xcd311b\">"
	 "</param><param name=\"allowFullScreen\" value=\"true\"></param>"
	 "<param name=\"allowscriptaccess\" value=\"always\"></param>"
	 "<embed src=\"http://www.youtube.com/v/"
	 id
	 "&amp;hl=en_US&amp;fs=1?rel=0\" type=\"application/x-shockwave-flash\" allowscriptaccess=\"always\" allowfullscreen=\"true\" width=\"" (number-to-string width) "\" height=\"" (number-to-string height) "\"></embed></object>")))

(defun vimeo (id)
  (let ((width 400)
		(height 225))
	(concat
	 "<object width=\"" (number-to-string width) "\" height=\"" (number-to-string height) "\">"
	 "<param name=\"allowfullscreen\" value=\"true\" />"
	 "<param name=\"allowscriptaccess\" value=\"always\" />"
	 "<param name=\"movie\" value=\"http://vimeo.com/moogaloop.swf?clip_id="
	 id
	 "&amp;server=vimeo.com&amp;show_title=1&amp;show_byline=1&amp;show_portrait=0&amp;color=&amp;fullscreen=1\" />"
	 "<embed src=\"http://vimeo.com/moogaloop.swf?clip_id="
	 id
	 "&amp;server=vimeo.com&amp;show_title=1&amp;show_byline=1&amp;show_portrait=0&amp;color=&amp;fullscreen=1\" type=\"application/x-shockwave-flash\" allowfullscreen=\"true\" allowscriptaccess=\"always\" width=\"" (number-to-string width) "\" height=\"" (number-to-string height) "\"></embed></object>")))

(defun list-join (list elt)
  (let (res)
    (dolist (i list)
      (push i res)
      (push elt res))
    (pop res)
    (nreverse res)))

(defun string-join (string separator)
  (apply 'concat (list-join string separator)))

(defun insert-element (elt pos list)
  (cond
   ((null list) (list elt))
   ((= pos 0) (push elt list))
   (t (cons (car list) (insert-element elt (- pos 1) (cdr list))))))

(defun concat-at (list pos new)
  (if (= pos 0)
	  (cons (concat (car list) new) (cdr list))
	(cons (car list) (concat-at (cdr list) (- pos 1) new))))

(defun make-thumb-url (image-url)
  "Appends \"-thumb\" to the name of the last dir of the passed url"
  (let ((tokens (split-string image-url "/")))
	(if (< (length tokens) 2)
		image-url
	  (string-join
	   (concat-at tokens (- (length tokens) 2) "-thumbs") "/"))))


(defun gallery (gallery-name image-entries)
  (let ((xmlgen-escape-elm-vals nil)) ;temporarily suppress escaping of content
	(concat
	 (apply
	  'concat
	  (mapcar
	   (lambda (image-entry)
		 (xmlgen
		  `(a :class ,(concat "gallery-image-" gallery-name)
			  :rel ,gallery-name
			  :href ,(car image-entry)
			  ,@(if (cadr image-entry) `(:title ,(cadr image-entry)) nil)
			  (img :class "thumb"
				   :src ,(make-thumb-url (car image-entry))
				   :alt ""))))
	   image-entries))
	 (xmlgen `(script
			   ,(concat
				 "$(\"a.gallery-image-" gallery-name "\").fancybox({'cyclic': true});"))))))

(provide 'stathis-blog)
