;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")
;;  removed (make-local-variable 'my-project-path) 

;;((nil . ((eval . (progn BODY)))))
((nil . ( (eval . (progn
		    (message "project root finding ... ")
		    ;; (setq sndr-project-root
		    ;; 	  (file-name-directory
		    ;; 	   (let ((d (dir-locals-find-file ".")))
		    ;; 	     (message (concat "project root is: " d))
                    ;;          (if (stringp d) d (car d)))))
		    (setq sndr-project-root (sndr-dir-locals-dir))
		    (setq bookmark-default-file (concat sndr-project-root ".project-bookmarks"))
		    (setq org-directory(concat sndr-project-root "notes") )
		    (setq org-default-notes-file (concat org-directory "/projectnotes.org"))
		    ;; root is where this file is
		    (message (concat "project root is: " sndr-project-root))
		    (setq uname (replace-regexp-in-string "[ \n]+$" "" (shell-command-to-string "uname")))
		    (define-key global-map "\em" 'make-without-asking)
;;		    (define-key global-map "\er" 'test-without-asking)
		    (define-key global-map "\er" 'start-run)
		    (define-key global-map "\eM" 'start-debug)

		    (setq cscope-initial-directory (concat sndr-project-root ".cscope" ))
		    (setq compilation-scroll-output t)
                    (setq compilation-scroll-output 'first-error)

		    (require 'thingatpt)
		    (defun find-this-on-ifcowl ()
		      "Find this thing on point on cpp-reference."
		      (interactive)
		      (let (myurl)
			(setq myurl (concat "http://ifcowl.openbimstandards.org/IFC4/index.html#" (thing-at-point 'symbol)))
			(browse-url myurl)
			)
		      )
 (defun find-this-on-cpp-reference ()
		      "Find this thing on point on cpp-reference."
		      (interactive)
		      (let (myurl)
			(setq myurl (concat "https://en.cppreference.com/mwiki/index.php?title=Special%3ASearch&search=" (thing-at-point 'symbol)))
			(browse-url myurl)
			)
		      )
		   (defun find-this-on-boost () 
		      "Find this thing on point in boost docs."
		      (interactive)
		      (let (myurl)
			(setq myurl (concat "https://cse.google.com/cse?cx=011577717147771266991:jigzgqluebe&q=" (thing-at-point 'symbol)))
			(browse-url myurl)
			)
		      ) 
		    (defun google-this-on-site (site)
		      "Find this thing on point on occt documents via google."
		      (interactive)
		      (let (myurl)
			(setq myurl (concat "https://encrypted.google.com/search?q=" (thing-at-point 'symbol) "+site%3D" site))
			(browse-url myurl)
			)
		      )
		    (defun google-occt-function ()
		      (interactive)
			( google-this-on-site "https://www.opencascade.com/doc/occt-7.3.0/refman/html/" )
			)
		    (defun just-open-building-smart-ifc4()
		      "Just open the building smart ifc4 spec site."
		      (interactive)
		      (browse-url "http://www.buildingsmart-tech.org/ifc/IFC4/final/html/")
		      )
		    
		    (global-unset-key (kbd "M-g"))		    
		    (global-set-key (kbd "M-g c") 'find-this-on-cpp-reference)		    
		    (global-set-key (kbd "M-g b") 'find-this-on-boost)		    
		    (global-set-key (kbd "M-g o") 'google-occt-function)		    
		    (global-set-key (kbd "M-g i") 'find-this-on-ifcowl)		    
		    (global-set-key (kbd "M-g B") 'just-open-building-smart-ifc4)
					; Microsoft Windows		    
		    (cond
		     ((string-equal system-type "windows-nt")
		      (progn
			(setq sander-makescript (concat sndr-project-root "0.3-win/build.bat"))
			(setq sander-runscript (concat sndr-project-root "0.3-win/run.bat"))
			(setq sander-debugscript (concat sndr-project-root "0.3-win/debug.bat"))
			(setq sndr-project-build-debug-dir (concat sndr-project-root "/IfcOpenshell/build-vs2017-x64-"))
			(setq sndr-project-build-debug-dir (concat sndr-project-build-debug-dir (system-name)))
			(setq tags-table-list (list
					       (expand-file-name (concat sndr-project-root ".TAGS-win") )
					       ) )
			
			(define-key global-map "\eT" '(lambda()
							(interactive)
							(compile (concat sndr-project-root "0.3-win/gentags.bat"))
							(other-window 1)
							))

			(message "Microsoft Windows"))
		      );; end windows

		     ((string-equal system-type "darwin") ; Mac OS X
		      (progn
			(message "Mac OS X"))
		      );;end macos

		     ((or (string-equal uname "MINGW64_NT-10.0")(string-equal system-type "gnu/linux") )
		      (progn
			(message "Linux/MSYS2")
			(setq sander-makescript (concat sndr-project-root "scipts/build.sh"))
			(setq sander-runscript (concat sndr-project-root "scripts/run.sh"))
			(setq sander-debugscript (concat sndr-project-root "scripts/debug.sh"))
			(setq sndr-project-build-debug-dir (concat sndr-project-root "build-"))
			(setq sndr-project-build-debug-dir (concat sndr-project-build-debug-dir uname))
			(irony-cdb-json-add-compile-commands-path
			 (concat sndr-project-root "IfcOpenShell")
			 (concat sndr-project-build-debug-dir "/compile_commands.json")
			 )
			(setq cmake-ide-build-dir sndr-project-build-debug-dir)
			(setq cmake-ide-project-dir (concat sndr-project-root "IfcOpenShell/cmake"))
			(setq tags-table-list (list
					       (expand-file-name (concat sndr-project-root ".TAGS-nix") )
					       ) )
			
			(define-key global-map "\eT" '(lambda()
							(interactive)
							(compile (concat sndr-project-root "scripts/gentags.sh"))
							(other-window 1)
							))
			(define-key global-map "\eM" '(lambda()
							(interactive)
							(setq exe "IfcConvert")
							(setq testfile
							      (concat
							       sndr-project-root
							       "testfiles/743--segfault-on-geometry.ifc" ))
							(gdb
							 (concat "gdb -i=mi -cd " sndr-project-build-debug-dir " --args " exe " -y " testfile " test.obj")
							 )
							))


	 		));; end linux/mingw
		     );; end cond
		    );; end progn
		);;end eval
	  );;end nil
      )
 )
