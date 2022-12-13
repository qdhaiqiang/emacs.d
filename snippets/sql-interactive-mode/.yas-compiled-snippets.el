;;; Compiled snippets and support files for `sql-interactive-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'sql-interactive-mode
                     '((">t" "create table test (id int(11) NOT NULL AUTO_INCREMENT, name varchar(20), PRIMARY KEY (\\`id\\`));" "test-table" nil nil nil "/Users/mahaiqiang/.emacs.d/snippets/sql-interactive-mode/test-table" nil nil)
                       ("insert" "insert into $1 ($2) values ($3);" "insert" nil nil nil "/Users/mahaiqiang/.emacs.d/snippets/sql-interactive-mode/insert" nil nil)
                       ("<db" "drop database $1;" "drop_db" nil nil nil "/Users/mahaiqiang/.emacs.d/snippets/sql-interactive-mode/+new-snippet+" nil nil)))


;;; Do not edit! File generated at Mon Dec 16 20:07:09 2019
