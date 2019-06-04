;;; docker-container.el --- Emacs interface to docker-container  -*- lexical-binding: t -*-

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>
;;         Yuki Inoue <inouetakahiroki@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 's)
(require 'dash)
(require 'json)
(require 'tablist)
(require 'transient)

(require 'docker-group)
(require 'docker-process)
(require 'docker-utils)

(defgroup docker-container nil
  "Docker container customization group."
  :group 'docker)

(defcustom docker-container-shell-file-name shell-file-name
  "Shell to use when entering containers.
For more information see the variable `shell-file-name'."
  :group 'docker-container
  :type 'string)

(defcustom docker-container-default-sort-key '("Image" . nil)
  "Sort key for docker containers.

This should be a cons cell (NAME . FLIP) where
NAME is a string matching one of the column names
and FLIP is a boolean to specify the sort order."
  :group 'docker-container
  :type '(cons (choice (const "Id")
                       (const "Image")
                       (const "Command")
                       (const "Created")
                       (const "Status")
                       (const "Ports")
                       (const "Names"))
               (choice (const :tag "Ascending" nil)
                       (const :tag "Descending" t))))

(defun docker-container--read-shell (&optional read-shell-name)
  "Reads a shell name if `read-shell-name' is truthy."
  (if read-shell-name (read-shell-command "Shell: ") docker-container-shell-file-name))

(defun docker-container-parse (line)
  "Convert a LINE from \"docker container ls\" to a `tabulated-list-entries' entry."
  (condition-case nil
      (let ((data (json-read-from-string line)))
        (setf (aref data 3) (format-time-string "%F %T" (date-to-time (aref data 3))))
        (list (aref data 6) data))
    (json-readtable-error
     (error "Could not read following string as json:\n%s" line))))

(defun docker-container-entries ()
  "Return the docker containers data for `tabulated-list-entries'."
  (let* ((fmt "[{{json .ID}},{{json .Image}},{{json .Command}},{{json .CreatedAt}},{{json .Status}},{{json .Ports}},{{json .Names}}]")
         (data (docker-run-docker "container ls" (docker-container-ls-arguments) (format "--format=\"%s\"" fmt)))
         (lines (s-split "\n" data t)))
    (-map #'docker-container-parse lines)))

(defun docker-container-refresh ()
  "Refresh the containers list."
  (setq tabulated-list-entries (docker-container-entries)))

(defun docker-container-read-name ()
  "Read an container name."
  (completing-read "Container: " (-map #'car (docker-container-entries))))

;;;###autoload
(defun docker-container-attach (container args)
  "Run \"docker attach ARGS CONTAINER\"."
  (interactive (list (docker-compose-read-name) (docker-container-attach-arguments)))
  (docker-run-docker "attach" args container))

;;;###autoload
(defun docker-container-eshell (container)
  "Open `eshell' in CONTAINER."
  (interactive (list (docker-container-read-name)))
  (let* ((container-address (format "docker:%s:/" container))
         (file-prefix (let ((prefix (file-remote-p default-directory)))
                        (if prefix
                            (format "%s|" (s-chop-suffix ":" prefix))
                          "/")))
         (default-directory (format "%s%s" file-prefix container-address))
         (eshell-buffer-name (generate-new-buffer-name (format "*eshell %s*" default-directory))))
    (eshell)))

;;;###autoload
(defun docker-container-find-directory (container directory)
  "Inside CONTAINER open DIRECTORY."
  (interactive
   (let* ((container-name (docker-container-read-name))
          (tramp-filename (read-directory-name "Directory: " (format "/docker:%s:/" container-name))))
     (with-parsed-tramp-file-name tramp-filename nil
       (list host localname))))
  (dired (format "/docker:%s:%s" container directory)))

(defalias 'docker-container-dired 'docker-container-find-directory)

;;;###autoload
(defun docker-container-find-file (container file)
  "Inside CONTAINER open FILE."
  (interactive
   (let* ((container-name (docker-container-read-name))
          (tramp-filename (read-file-name "File: " (format "/docker:%s:/" container-name))))
     (with-parsed-tramp-file-name tramp-filename nil
       (list host localname))))
  (find-file (format "/docker:%s:%s" container file)))

;;;###autoload
(defun docker-container-shell (container &optional read-shell)
  "Open `shell' in CONTAINER."
  (interactive (list
                (docker-container-read-name)
                current-prefix-arg))
  (let* ((shell-file-name (docker-container--read-shell read-shell))
         (container-address (format "docker:%s:/" container))
         (file-prefix (let ((prefix (file-remote-p default-directory)))
                        (if prefix
                            (format "%s|" (s-chop-suffix ":" prefix))
                          "/")))
         (default-directory (format "%s%s" file-prefix container-address)))
    (shell (generate-new-buffer (format "*shell %s*" default-directory)))))

;;;###autoload
(defun docker-diff (name)
  "Diff the container named NAME."
  (interactive (list (docker-container-read-name)))
  (docker-utils-with-buffer (format "diff %s" name)
    (insert (docker-run-docker "diff" name))))

;;;###autoload
(defun docker-inspect (name)
  "Inspect the container named NAME."
  (interactive (list (docker-container-read-name)))
  (docker-utils-with-buffer (format "inspect %s" name)
    (insert (docker-run-docker "inspect" name))
    (json-mode)))

;;;###autoload
(defun docker-kill (name &optional signal)
  "Kill the container named NAME using SIGNAL."
  (interactive (list (docker-container-read-name)))
  (docker-run-docker "kill" (when signal (format "-s %s" signal)) name))

;;;###autoload
(defun docker-logs (name &optional follow)
  "Show the logs from container NAME.

If FOLLOW is set, run in `async-shell-command'."
  (interactive (list (docker-container-read-name)))
  (if follow
      (async-shell-command
       (format "%s logs -f %s" docker-command name)
       (generate-new-buffer (format "* docker logs %s *" name)))
    (docker-utils-with-buffer (format "logs %s" name)
      (insert (docker-run-docker "logs" name)))))

;;;###autoload
(defun docker-pause (name)
  "Pause the container named NAME."
  (interactive (list (docker-container-read-name)))
  (docker-run-docker "pause" name))

;;;###autoload
(defun docker-rename (container name)
  "Rename CONTAINER using NAME."
  (interactive (list (docker-container-read-name) (read-string "Name: ")))
  (docker-run-docker "rename" container name))

;;;###autoload
(defun docker-restart (name &optional timeout)
  "Restart the container named NAME.

TIMEOUT is the number of seconds to wait for the container to stop before killing it."
  (interactive (list (docker-container-read-name) current-prefix-arg))
  (docker-run-docker "restart" (when timeout (format "-t %d" timeout)) name))

;;;###autoload
(defun docker-rm (name &optional force link volumes)
  "Remove the container named NAME.

With prefix argument, sets FORCE to true.

Force the removal even if the container is running when FORCE is set.
Remove the specified link and not the underlying container when LINK is set.
Remove the volumes associated with the container when VOLUMES is set."
  (interactive (list (docker-container-read-name) current-prefix-arg))
  (docker-run-docker "rm" (when force "-f") (when link "-l") (when volumes "-v") name))

;;;###autoload
(defun docker-start (name)
  "Start the container named NAME."
  (interactive (list (docker-container-read-name)))
  (docker-run-docker "start" name))

;;;###autoload
(defun docker-stop (name &optional timeout)
  "Stop the container named NAME.

TIMEOUT is the number of seconds to wait for the container to stop before killing it."
  (interactive (list (docker-container-read-name) current-prefix-arg))
  (docker-run-docker "stop" (when timeout (format "-t %d" timeout)) name))

;;;###autoload
(defun docker-unpause (name)
  "Unpause the container named NAME."
  (interactive (list (docker-container-read-name)))
  (docker-run-docker "unpause" name))

(defun docker-container-attach-selection ()
  "Run \"docker attach\" with the containers selection."
  (interactive)
  (let ((default-directory (if (and docker-run-as-root
                                    (not (file-remote-p default-directory)))
                               "/sudo::"
                             default-directory)))
    (--each (docker-utils-get-marked-items-ids)
      (async-shell-command
       (format "%s attach %s %s" docker-command (s-join " " (transient-args 'docker-container-attach)) it)
       (generate-new-buffer (format "*attach %s*" it))))))

(defun docker-container-cp-from-selection (container-path host-path)
  "Run \"docker cp\" from CONTAINER-PATH to HOST-PATH for selected container."
  (interactive "sContainer path: \nFHost path: ")
  (--each (docker-utils-get-marked-items-ids)
    (docker-run-docker "cp" (concat it ":" container-path) host-path)))

(defun docker-container-cp-to-selection (host-path container-path)
  "Run \"docker cp\" from HOST-PATH to CONTAINER-PATH for selected containers."
  (interactive "fHost path: \nsContainer path: ")
  (--each (docker-utils-get-marked-items-ids)
    (docker-run-docker "cp" host-path (concat it ":" container-path))))

(defun docker-container-diff-selection ()
  "Run `docker-diff' on the containers selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-utils-with-buffer (format "diff %s" it)
      (insert (docker-run-docker "diff" (transient-args 'docker-container-diff) it)))))

(defun docker-container-eshell-selection ()
  "Run `docker-container-eshell' on the containers selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-container-eshell it)))

(defun docker-container-find-file-selection (path)
  "Run `docker-container-find-file' for PATH on the containers selection."
  (interactive "sPath: ")
  (--each (docker-utils-get-marked-items-ids)
    (docker-container-find-file it path)))

(defun docker-container-inspect-selection ()
  "Run `docker-inspect' on the containers selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-utils-with-buffer (format "inspect %s" it)
      (insert (docker-run-docker "inspect" (transient-args 'docker-container-inspect) it))
      (json-mode))))

(defun docker-container-logs-selection ()
  "Run \"docker logs\" on the containers selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (async-shell-command
     (format "%s logs %s %s" docker-command (s-join " " (transient-args 'docker-container-logs)) it)
     (generate-new-buffer (format "* docker logs %s *" it)))))

(defun docker-container-rename-selection ()
  "Rename containers."
  (interactive)
  (docker-utils-select-if-empty)
  (--each (docker-utils-get-marked-items-ids)
    (docker-rename it (read-string (format "New name for %s: " it))))
  (tablist-revert))

(defun docker-container-shell-selection (prefix)
  "Run `docker-container-shell' on the containers selection."
  (interactive "P")
  (--each (docker-utils-get-marked-items-ids)
    (docker-container-shell it prefix)))

(define-transient-command docker-container-attach ()
  "Transient for attaching to containers."
  :man-page "docker-attach"
  ["Arguments"
   ("-n" "No STDIN" "--no-stdin")
   ("-d" "Key sequence for detaching" "--detach-keys=" read-string)]
  ["Actions"
   ("a" "Attach" docker-container-attach-selection)]
  (interactive)
  (docker-utils-select-if-empty)
  (transient-setup 'docker-container-attach))

(define-transient-command docker-container-cp ()
  "Transient for copying files from/to containers."
  :man-page "docker-cp"
  ["Actions"
   ("f" "Copy From" docker-container-cp-from-selection)
   ("t" "Copy To" docker-container-cp-to-selection)])

(define-transient-command docker-container-diff ()
  "Transient for showing containers diffs."
  :man-page "docker-diff"
  ["Actions"
   ("d" "Diff" docker-container-diff-selection)])

(define-transient-command docker-container-find-file ()
  "Transient for opening containers files."
  ["Actions"
   ("f" "Open file" docker-container-find-file-selection)])

(define-transient-command docker-container-inspect ()
  "Transient for inspecting containers."
  :man-page "docker-inspect"
  ["Actions"
   ("I" "Inspect" docker-container-inspect-selection)])

(define-transient-command docker-container-kill ()
  "Transient for kill signaling containers"
  :man-page "docker-kill"
  ["Arguments"
   ("-s" "Signal" "-s " read-string)]
  ["Actions"
   ("K" "Kill" docker-utils-run-action-on-selection)])

(define-transient-command docker-container-logs ()
  "Transient for showing containers logs."
  :man-page "docker-logs"
  ["Arguments"
   ("-f" "Follow" "-f")]
  ["Actions"
   ("L" "Logs" docker-container-logs-selection)])

(defun docker-container-ls-arguments ()
  "Return the latest used arguments in the `docker-container-ls' transient."
  (car (alist-get 'docker-container-ls transient-history)))

(define-transient-command docker-container-ls ()
  "Transient for listing containers."
  :man-page "docker-container-ls"
  :value '("--all")
  ["Arguments"
   ("-N" "Last" "--last " transient-read-number-N0)
   ("-a" "All" "--all")
   ("-e" "Exited containers" "--filter status=exited")
   ("-f" "Filter" "--filter " read-string)
   ("-n" "Don't truncate" "--no-trunc")]
  ["Actions"
   ("l" "List" tablist-revert)])

(define-transient-command docker-container-pause ()
  "Transient for pauseing containers."
  :man-page "docker-pause"
  ["Actions"
   ("P" "Pause" docker-utils-run-action-on-selection)
   ("U" "Unpause" docker-utils-run-action-on-selection)])

(define-transient-command docker-container-restart ()
  "Transient for restarting containers."
  :man-page "docker-restart"
  ["Arguments"
   ("-t" "Timeout" "-t " transient-read-number-N0)]
  ["Actions"
   ("R" "Restart" docker-utils-run-action-on-selection)])

(define-transient-command docker-container-rm ()
  "Transient for removing containers."
  :man-page "docker-rm"
  ["Arguments"
   ("-f" "Force" "-f")
   ("-v" "Volumes" "-v")]
  ["Actions"
   ("D" "Remove" docker-utils-run-action-on-selection)])

(define-transient-command docker-container-shell ()
  "Transient for doing M-x `shell'/`eshell' to containers."
  ["Actions"
   ("b" "Shell" docker-container-shell-selection)
   ("e" "Eshell" docker-container-eshell-selection)])

(define-transient-command docker-container-start ()
  "Transient for starting containers."
  :man-page "docker-start"
  ["Actions"
   ("S" "Start" docker-utils-run-action-on-selection)])

(define-transient-command docker-container-stop ()
  "Transient for stoping containers."
  :man-page "docker-stop"
  ["Arguments"
   ("-t" "Timeout" "-t " transient-read-number-N0)]
  ["Actions"
   ("O" "Stop" docker-utils-run-action-on-selection)])

(define-transient-command docker-container-help ()
  "Help transient for docker containers."
  ["Docker containers help"
   ("C" "Copy"       docker-container-cp)
   ("D" "Remove"     docker-container-rm)
   ("I" "Inspect"    docker-container-inspect)
   ("K" "Kill"       docker-container-kill)
   ("L" "Logs"       docker-container-logs)
   ("O" "Stop"       docker-container-stop)
   ("P" "Pause"      docker-container-pause)
   ("R" "Restart"    docker-container-restart)
   ("S" "Start"      docker-container-start)
   ("a" "Attach"     docker-container-attach)
   ("b" "Shell"      docker-container-shell)
   ("d" "Diff"       docker-container-diff)
   ("f" "Find file"  docker-container-find-file)
   ("l" "List"       docker-container-ls)
   ("r" "Rename"     docker-container-rename-selection)])

(defvar docker-container-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'docker-container-help)
    (define-key map "C" 'docker-container-cp)
    (define-key map "D" 'docker-container-rm)
    (define-key map "I" 'docker-container-inspect)
    (define-key map "K" 'docker-container-kill)
    (define-key map "L" 'docker-container-logs)
    (define-key map "O" 'docker-container-stop)
    (define-key map "P" 'docker-container-pause)
    (define-key map "R" 'docker-container-restart)
    (define-key map "S" 'docker-container-start)
    (define-key map "a" 'docker-container-attach)
    (define-key map "b" 'docker-container-shell)
    (define-key map "d" 'docker-container-diff)
    (define-key map "f" 'docker-container-find-file)
    (define-key map "l" 'docker-container-ls)
    (define-key map "r" 'docker-container-rename-selection)
    map)
  "Keymap for `docker-container-mode'.")

;;;###autoload
(defun docker-containers ()
  "List docker containers."
  (interactive)
  (docker-utils-pop-to-buffer "*docker-containers*")
  (docker-container-mode)
  (tablist-revert))

(define-derived-mode docker-container-mode tabulated-list-mode "Containers Menu"
  "Major mode for handling a list of docker containers."
  (setq tabulated-list-format [("Id" 16 t)("Image" 15 t)("Command" 30 t)("Created" 23 t)("Status" 20 t)("Ports" 10 t)("Names" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key docker-container-default-sort-key)
  (add-hook 'tabulated-list-revert-hook 'docker-container-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'docker-container)

;;; docker-container.el ends here
