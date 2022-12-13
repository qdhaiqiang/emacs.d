;;; Compiled snippets and support files for `clojurescript-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'clojurescript-mode
                     '((">sub" "(rf/reg-sub\n :${1::key}\n (fn [db]\n   (get-in db [$2] ${3:default})))" "reg-sub" nil nil nil "/Users/mahaiqiang/.emacs.d/snippets/clojurescript-mode/reg-sub" nil nil)
                       ("<l" "(let x y $0 $1)" "let" nil nil nil "/Users/mahaiqiang/.emacs.d/snippets/clojurescript-mode/mlet" nil nil)
                       (">ctl" ";;$1\n(kf/reg-controller\n  :${2:key}\n  {:params (fn [route]\n               (when (-> route :path-params :path (= \"/${3:path}\")) true))\n   :start (fn [_]\n              (rf/dispatch [:${4:event}]))})" "kfcontroller" nil nil nil "/Users/mahaiqiang/.emacs.d/snippets/clojurescript-mode/kfcontroller" nil nil)
                       (">inc" "(:require\n   [\"moment\" :as moment]\n   [reagent.core :as r]\n   [re-frame.core :as rf]\n   [com.rpl.specter :as st]\n   [goog.string :as gstring]\n   [redcreation.redant.reagent :as ant]\n   [custom-manage.craftwork-new.craftwork-events]\n   [custom-manage.components.common-page :as page]\n   [custom-manage.components.category-select :as category]\n   [custom-manage.components.hc-upload :refer [image-upload]])" "include" nil nil nil "/Users/mahaiqiang/.emacs.d/snippets/clojurescript-mode/include" nil nil)
                       ("gstring" "[goog.string :as gstring]" "gstring" nil nil nil "/Users/mahaiqiang/.emacs.d/snippets/clojurescript-mode/gstring" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'clojurescript-mode
                     '(("sub" "(rf/reg-sub\n :{$1:...}\n (fn [db]\n   (get-in db [$2] {$3:default})))" "rf/reg-sub" nil nil nil "/Users/mahaiqiang/.emacs.d/snippets/clojurescript-mode/rf/reg-sub" nil nil)))


;;; Do not edit! File generated at Mon Dec 16 20:07:09 2019
