;;; docker-machine.el --- Emacs interface to docker-machine  -*- lexical-binding: t -*-

;; Author: Ben Swift <ben@benswift.me>

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
(require 'tablist)
(require 'transient)

(require 'docker-group)
(require 'docker-utils)

(defgroup docker-machine nil
  "Docker machine customization group."
  :group 'docker)

(defcustom docker-machine-default-sort-key '("Name" . nil)
  "Sort key for docker machines.

This should be a cons cell (NAME . FLIP) where
NAME is a string matching one of the column names
and FLIP is a boolean to specify the sort order."
  :group 'docker-machine
  :type '(cons (choice (const "Name")
                       (const "Active")
                       (const "Driver")
                       (const "State")
                       (const "URL")
                       (const "Swarm")
                       (const "Docker")
                       (const "Errors"))
               (choice (const :tag "Ascending" nil)
                       (const :tag "Descending" t))))

(defun docker-machine-parse (line)
  "Convert a LINE from \"docker machine ls\" to a `tabulated-list-entries' entry."
  (let ((data (s-split "\t" line)))
    (list (car data) (apply #'vector data))))

(defun docker-machine-entries ()
  "Return the docker machines data for `tabulated-list-entries'."
  (let* ((fmt "{{.Name}}\\t{{.Active}}\\t{{.DriverName}}\\t{{.State}}\\t{{.URL}}\\t{{.Swarm}}\\t{{.DockerVersion}}\\t{{.Error}}")
         (data (docker-machine-run "ls" docker-machine-ls-arguments (format "--format=\"%s\"" fmt)))
         (lines (s-split "\n" data t)))
    (-map #'docker-machine-parse lines)))

(defun docker-machine-refresh ()
  "Refresh the machines list."
  (setq tabulated-list-entries (docker-machine-entries)))

(defun docker-machine-read-name ()
  "Read a machine name."
  (completing-read "Machine: " (-map #'car (docker-machine-entries))))

(defun docker-machine-run (action &rest args)
  "Execute \"docker-machine ACTION\" using ARGS."
  (let ((command (format "docker-machine %s %s" action (s-join " " (-flatten (-non-nil args))))))
    (message command)
    (shell-command-to-string command)))

;;;###autoload
(defun docker-machine-config (name)
  "Print the connection config for the machine NAME."
  (interactive (list (docker-machine-read-name)))
  (docker-machine-run "config" name))

;;;###autoload
(defun docker-machine-create (name driver)
  "Create a machine NAME using DRIVER."
  (interactive "sName: \nsDriver: ")
  (docker-machine-run "create" name "-d" driver))

;;;###autoload
(defun docker-machine-env (name)
  "Parse and set environment variables from \"docker-machine env NAME\" output."
  (interactive (list (docker-machine-read-name)))
  (--each-while
      (s-lines (docker-machine-run "env" name))
      (s-prefix? "export" it)
    (docker-machine-env-export it)))

(defun docker-machine-env-export (line)
  "Export the env for LINE."
  (let ((index (s-index-of "=" line)))
    (unless index
      (error (format "Cannot find separator in %s" line)))
    (setenv (substring line (length "export ") index) (substring line (+ 2 index) -1))))

;;;###autoload
(defun docker-machine-inspect (name)
  "Inspect information about the machine NAME."
  (interactive (list (docker-machine-read-name)))
  (docker-machine-run "inspect" name))

;;;###autoload
(defun docker-machine-ip (name)
  "Get the IP address of the machine NAME."
  (interactive (list (docker-machine-read-name)))
  (docker-machine-run "ip" name))

;;;###autoload
(defun docker-machine-kill (name)
  "Kill the machine NAME."
  (interactive (list (docker-machine-read-name)))
  (docker-machine-run "kill" name))

;;;###autoload
(defun docker-machine-restart (name)
  "Restart the machine NAME."
  (interactive (list (docker-machine-read-name) current-prefix-arg))
  (docker-machine-run "restart" name))

;;;###autoload
(defun docker-machine-rm (name &optional force)
  "Destroy or uncommand the machine NAME.  If FORCE is set, use \"--force\"."
  (interactive (list (docker-machine-read-name) current-prefix-arg))
  (docker-machine-run "rm" (when force "--force") name))

;;;###autoload
(defun docker-machine-start (name)
  "Start the machine NAME."
  (interactive (list (docker-machine-read-name)))
  (docker-machine-run "start" name))

;;;###autoload
(defun docker-machine-status (name)
  "Get the status of the machine NAME."
  (interactive (list (docker-machine-read-name)))
  (docker-machine-run "status" name))

;;;###autoload
(defun docker-machine-stop (name)
  "Stop the machine NAME."
  (interactive (list (docker-machine-read-name) current-prefix-arg))
  (docker-machine-run "stop" name))

;;;###autoload
(defun docker-machine-upgrade (name)
  "Upgrade the machine NAME to the latest version of Docker."
  (interactive (list (docker-machine-read-name)))
  (docker-machine-run "upgrade" name))

(defun docker-machine-env-selection ()
  "Run \"docker-machine env\" on selected machine."
  (interactive)
  (let ((marked (docker-utils-get-marked-items-ids)))
    (when (/= (length marked) 1)
      (error "Can only set environment vars for one machine at a time"))
    (docker-machine-env (car marked))
    (tablist-revert)))

(defun docker-machine-restart-selection ()
  "Run `docker-machine-restart' on the machines selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-machine-run "restart" (docker-machine-restart-arguments) it))
  (tablist-revert))

(defun docker-machine-rm-selection ()
  "Run `docker-machine-rm' on the machines selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-machine-run "rm" (docker-machine-rm-arguments) it))
  (tablist-revert))

(defun docker-machine-start-selection ()
  "Run `docker-machine-start' on the machines selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-machine-run "start" (docker-machine-start-arguments) it))
  (tablist-revert))

(defun docker-machine-stop-selection ()
  "Run `docker-machine-stop' on the machines selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-machine-run "stop" (docker-machine-stop-arguments) it))
  (tablist-revert))

(define-transient-command docker-machine-env ()
  "Transient for setting up environment variables."
  :man-page "docker-machine-env"
  ["Actions"
   ("E" "Env" docker-machine-env-selection)])

(defun docker-machine-ls-arguments ()
  "Return the latest used arguments in the `docker-machine-ls' transient."
  (car (alist-get 'docker-machine-ls transient-history)))

(define-transient-command docker-machine-ls ()
  "Transient for listing machines."
  :man-page "docker-machine-ls"
  ["Arguments"
   ("-f" "Filter" "--filter " read-string)
   ("-t" "Timeout" "--timeout " transient-read-number-N0)]
  ["Actions"
   ("l" "List" tablist-revert)])

(define-transient-command docker-machine-restart ()
  "Transient for restarting machines."
  :man-page "docker-machine-restart"
  ["Actions"
   ("R" "Restart" docker-machine-restart-selection)])

(define-transient-command docker-machine-rm ()
  "Transient for removing machines."
  :man-page "docker-machine-rm"
  :value '("-y")
  ["Arguments"
   ("-f" "Force" "-f")
   ("-y" "Automatic yes" "-y")]
  ["Actions"
   ("D" "Remove" docker-machine-rm-selection)])

(define-transient-command docker-machine-start ()
  "Transient for starting machines."
  :man-page "docker-machine-start"
  ["Actions"
   ("S" "Start" docker-machine-start-selection)])

(define-transient-command docker-machine-stop ()
  "Transient for stoping machines."
  :man-page "docker-machine-stop"
  ["Actions"
   ("O" "Stop" docker-machine-stop-selection)])

(define-transient-command docker-machine-help ()
  "Help transient for docker machine."
  ["Docker machines help"
   ("C" "Create"     docker-machine-create)
   ("D" "Remove"     docker-machine-rm)
   ("E" "Env"        docker-machine-env)
   ("O" "Stop"       docker-machine-stop)
   ("R" "Restart"    docker-machine-restart)
   ("S" "Start"      docker-machine-start)
   ("l" "List"       docker-machine-ls)])

(defvar docker-machine-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'docker-machine-help)
    (define-key map "C" 'docker-machine-create)
    (define-key map "D" 'docker-machine-rm)
    (define-key map "E" 'docker-machine-env)
    (define-key map "O" 'docker-machine-stop)
    (define-key map "R" 'docker-machine-restart)
    (define-key map "S" 'docker-machine-start)
    (define-key map "l" 'docker-machine-ls)
    map)
  "Keymap for `docker-machine-mode'.")

;;;###autoload
(defun docker-machines ()
  "List docker machines."
  (interactive)
  (docker-utils-pop-to-buffer "*docker-machines*")
  (docker-machine-mode)
  (tablist-revert))

(define-derived-mode docker-machine-mode tabulated-list-mode "Machines Menu"
  "Major mode for handling a list of docker machines."
  (setq tabulated-list-format [("Name" 16 t)("Active" 7 t)("Driver" 12 t)("State" 12 t)("URL" 30 t)("Swarm" 10 t)("Docker" 10 t)("Errors" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key docker-machine-default-sort-key)
  (add-hook 'tabulated-list-revert-hook 'docker-machine-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'docker-machine)

;;; docker-machine.el ends here
