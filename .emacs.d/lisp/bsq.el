(setenv "bsq" "~/devel/work/bsq/")

(org-add-link-type "j" 'jira-open)

(defun jira-open (id)
  (browse-url (concat "https://bare-square.atlassian.net/browse/" id)))

(provide 'bsq)
