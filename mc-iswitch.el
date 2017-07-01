;; mc-iswitch : An isearch-like version of switch-to-buffer.

;; This software is distributed under the "Simplified BSD license":
;;
;; Copyright Michael Cook <michael@waxrat.com>. All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;;    1. Redistributions of source code must retain the above copyright notice,
;;       this list of conditions and the following disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;; ----------------------------------------------------------------------------

;; 04/27/2009 Limit the length of the prompt.
;; 08/12/96 Removed support for EMACS-18.
;; 08/12/96 Incorporated some changes from Dan Schmidt <dfan(@)lglass.com>:
;;          C-k kills first buffer.
;;          Use read-event instead of read-char.
;; 04/05/96 Set the search string to the current buffer's basename.
;; 02/14/96 Tweak for Emacs 19.30: buffer-list now includes /all/ live buffers.
;; 05/19/94 Added tweaks for Lucid Emacs (thanks to
;;          <rwhitby(@)research.canon.oz.au> (Rod Whitby)).  Beefed-up the
;;          documentation strings.  Bug fix: pressing TAB at `foo{,bar}' would
;;          cause an args-out-of-range error.
;; 10/26/93 Pressing TAB when there is no common string acts as if
;;          you had typed the next character of the first buffer name.
;;          Pressing SPC moves the first matched buffer name to the end of the
;;          list.  C-q quoting added.  Pressing C-f invokes `find-file' using
;;          what you've typed so far as the initial text.
;; 12/14/92 Bug fix: "If you hit tab twice, you get a type-mismatch error."
;;          Thanks to <wmesard(@)waimea.stanford.edu>.
;; 12/06/92 Added mc-iswitch-to-buffer-other-window and
;;          mc-iswitch-to-buffer-other-frame.  Thanks to
;;          <liblit(@)cs.psu.edu> (Benjamin R Liblit).
;; 12/04/92 More information in the prompt: () and {}.
;;

;; See also the `icomplete.el' package, which was inspired by this code.

(defun mc-iswitch-to-buffer (arg)
  "An isearch-like version of `switch-to-buffer'.
With prefix arg, switch to buffer in other window.

A list of buffer names displays in the echo area, like this:

   Switch to buffer: {foo,barton,bark}

As you type characters, the prompt updates to show only those
buffers that match what you've typed.  For example, if you
type `b', the prompt changes to:

   Switch to buffer: b(ar){ton,k}

The part in parentheses `ar' is common to all of the matched buffer names.
Press TAB to add the common part `ar' to the match string, or (if there is no
common part) to add the character after the `{' to the match string.
Press RET to select the first matched buffer `barton'.
Press DEL to remove the last character from the match string.
Press SPC to rotate the buffer list.
Press ? to see the list of matched buffer names in a window.
Press C-f to invoke `find-file' using the match string as the initial text.
Press C-b to set the search string to the current buffer's basename.
Press C-k to kill the first matched buffer.
Press C-q to quote a character into the match string.
Other keystrokes terminate this function.

In particular, \\[mc-iswitch-to-buffer] RET is a convenient way to switch
between two buffers.  Mix with \\[mc-iswitch-to-buffer] SPC RET to switch
among three buffers, etc."

  (interactive "P")
  (if arg
      (mc-iswitch " in other window" 'switch-to-buffer-other-window)
    (mc-iswitch "" 'switch-to-buffer)))

(defun mc-iswitch-to-buffer-other-window ()
   "An isearch-like version of `switch-to-buffer-other-window'.
See \\[mc-iswitch-to-buffer]."
   (interactive)
   (mc-iswitch-to-buffer t))

(defun mc-iswitch-to-buffer-other-frame ()
  "An isearch-like version of `find-buffer-other-frame'.
See \\[mc-iswitch-to-buffer]."
  (interactive)
  (mc-iswitch " in other frame" 'switch-to-buffer-other-frame))

(defun mc-iswitch (prompt switch-technique)
  (let ((buf (save-window-excursion
	       (catch 'iswitch-done
		 (let ((bufs (apply 'append
				    (mapcar
				     '(lambda (b)
					(cond ((eq b (current-buffer))
					       nil)
					      ((string-equal ""
							     (buffer-name b))
					       nil)
					      ((= (aref (buffer-name b) 0) ? )
					       nil)
					      (t
					       (list (cons (buffer-name b)
							   b)))))
				     (buffer-list))))
		       ;(completion-ignore-case t)
		       (name '("")))
		   (while t
		     (or (input-pending-p)
			 (let (message-log-max) ; don't update *Messages*
			   (message (concat "Switch to buffer" prompt ": %s")
				    (mc-iswitch-prompt (car name) bufs))))
		     (let ((event (read-event)))
		       (cond
			;;
			;; DEL: remove the last character that was added to
			;; the match string.
			;;
			((or (eq event ?\177)
			     (eq event 'delete)
			     (eq event 'backspace))
			 (if (cdr name)
			     (setq name (cdr name))))
			;;
			;; TAB: take the first character of the first buffer
			;; name and append it to the match string.
			;;
			((or (eq event ?\t)
			     (eq event 'tab))
			 (let ((most (try-completion (car name) bufs)))
			   (cond ((not (stringp most))
				  (beep)) ;there are no matches
				 ((> (length most) (length (car name)))
				  (setq name (cons most name)))
				 ;; Take the first character that displays
				 ;; after the `{' and append it to the match
				 ;; string.
				 ((let ((first (car (all-completions
						     (car name) bufs))))
				    (if (< (length most) (length first))
					(setq name (cons
						    (concat
						     (car name)
						     (substring
						      first (length most)
						      (1+ (length most))))
						    name))
				      (beep)))))))
			;;
			;; SPC: Move the first-matched buffer name to the end
			;; of the buffer list.
			;;
			((eq event ? )
			 (let* ((a (car (all-completions
					 (car name) bufs)))
				(b (assq a bufs)))
			   (setq bufs (append (delq b bufs)
					      (list b)))))
			;;
			;; ?: Show a list of the buffer names that match.
			;;
			((eq event ??)
			 (let ((comps (all-completions (car name) bufs)))
			   (cond ((null comps)
				  (message "No matches.")
				  (sit-for 2))
				 (t
				  (with-output-to-temp-buffer "*Help*"
				    (display-completion-list comps))))))
			;;
			;; RET: switch to the first buffer in the match list.
			;;
			((or (eq event ?\C-m)
			     (eq event 'return))
			 (let ((match (car (all-completions
					    (car name) bufs))))
			   (and match
				(get-buffer match)
				(throw 'iswitch-done (get-buffer match)))
			   (beep)))
			;;
			;; C-b: set the search string to the current basename.
			;;
			((eq event ?\C-b)
			 (setq name (cons (concat (car name)
						  (file-name-nondirectory
						   (or (buffer-file-name)
						       (buffer-name))))
					  name)))
			;;
			;; C-k: kill the first buffer in the match list.
			;;
			((eq event ?\C-k)
			 (let ((match (car (all-completions
					    (car name) bufs))))
			   (and match
				(kill-buffer match)
				(setq bufs (delq (assq match bufs) bufs)))))
			;;
			;; C-f: take what we've typed so far (even if it does
			;; not match any buffer names) and use it to prompt
			;; for a file to find.  (If we've type most of a
			;; file's name and then realize that we don't have
			;; that file in a buffer yet, we can begin a find-file
			;; without losing what we've typed.)
			;;
			((eq event ?\C-f)
			 (throw 'iswitch-done
				(find-file-noselect
				 (expand-file-name
				  (let ((n (or (car (all-completions
						     (car name) bufs))
					       (car name))))
				    (read-file-name
				     (concat "Find file" prompt ": ")
				     nil (concat default-directory n)
				     nil n))))))
			;;
			;; C-q: quote a character into the match string
			;;
			((eq event ?\C-q)
			 (setq name
			       (cons (concat
				      (car name)
				      (char-to-string
				       (read-quoted-char
					(concat "Switch to buffer" prompt
						": " (car name) "^Q"))))
				     name)))
			;;
			;; Append this character to the match string.
			;;
			((and (integerp event)
			      (> event ? )
			      (<= event ?~))
			 (setq name (cons (concat (car name)
						  (char-to-string event))
					  name)))
			;;
			;; Push-back this event and then exit.
			;;
			(t
			 (cond ((boundp 'unread-command-event)  ; LEMACS-19
				(setq unread-command-event event))
			       (t
				(setq unread-command-events
				      (cons event unread-command-events))))
			 (throw 'iswitch-done nil))))))))))
    (if buf
	(let ((bfn (buffer-file-name buf)))
	  (funcall switch-technique buf)
	  (if bfn
	      (message "File: %s" (buffer-file-name buf))
	    (message "Buffer: %s" (buffer-name buf)))))))

(defun mc-iswitch-prompt (name bufs)
  ;;
  ;; Build (and return) the prompt string.  NAME is the current match string
  ;; (what we've typed so far).  BUFS is the buffer list.
  ;;
  (let ((comps (all-completions name bufs)))
    (cond ((null comps)
	   (concat name " [No matches]"))
	  ((null (cdr comps))		; One match.
	   (concat
	    name
	    (and (> (length (car comps))
		    (length name))
		 (concat "(" (substring (car comps) (length name)) ")"))
	    " [Matched]"))
	  (t				; Multiple matches.
	   (let* ((most (try-completion name bufs))
		  (p (concat
		      name
		      (and (> (length most) (length name))
			   (concat "(" (substring most (length name)) ")"))
		      "{"
		      (apply 'concat
			     (cdr (apply 'append
					 (mapcar '(lambda (com)
						    (list ","
							  (substring
							   com (length most))))
						 comps))))
		      "}"))
		  (m (* 4 (frame-width))))
	     (if (< (length p) m)
		 p
	       (concat (substring p 0 m) "...")))))))

;;(mc-iswitch-prompt "abcdefg" '(("abcdefghi" . 1) ("abcdefghj" . 1) ("abcdefklm" . 2) ("abcdefnop" . 3)))
