(defpackage #:weblocks-demo/app
  (:use #:cl)
  (:import-from #:weblocks/server)
  (:import-from #:weblocks-lass)
  (:import-from #:weblocks-parenscript)
  (:import-from #:weblocks/app
                #:defapp)
  (:import-from #:weblocks/widget
                #:defwidget
                #:update)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:weblocks-ui/form
                #:with-html-form
                #:render-button
                #:render-form-and-button))
(in-package weblocks-demo/app)

(defvar *port* 8080)

(defapp meetup
  :prefix "/")


(defwidget registration ()
  ((state :type (or (member :name :email :how-to-know :confirm :done)
                    null)
          :accessor get-state
          :initform nil)
   (name
    :initform nil
    :accessor get-name)
   (email
    :initform nil
    :accessor get-email)
   (how-to-know
    :initform nil
    :accessor get-how-to-know)))


(defmethod weblocks/session:init ((app meetup))
  (make-instance 'registration))


(defmethod weblocks/widget:render ((widget registration))
  (flet ((switch-to-state (state)
           (lambda (&rest rest)
             (declare (ignorable rest))
             (setf (get-state widget) state)
             (update widget))))
    (with-html
      (:h1 "Зарегистрируйся на Moscow Common Lisp Meetup!")
      (with-accessors ((state get-state))
          widget
        (cond
          ((null state)
           (:p "Чтобы зарегистрироваться, нажми кнопку:")
           (render-form-and-button "Кнопка"
                                   (lambda (&rest rest)
                                     (declare (ignorable rest))
                                     (setf state :name)
                                     (update widget))))
         
          ((eql state :name)
           (:p "Как тебя зовут?")
           (with-html-form (:post (lambda (&key value &allow-other-keys)
                                    (setf state :email)
                                    (setf (get-name widget) value)
                                    (update widget)))
             (:input :type "text"
                     :name "value"
                     :placeholder "Василиса Пупкина")
             (render-button "Далее")))
         
          ((eql state :email)
           (:p "Email для связи:")
           (with-html-form (:post (lambda (&key value &allow-other-keys)
                                    (setf state :how-to-know)
                                    (setf (get-email widget) value)
                                    (update widget)))
             (:input :type "text"
                     :name "value"
                     :placeholder "vasily-pupkin@mail.ru")
             (render-button "Далее")))
         
          ((eql state :how-to-know)
           (:p "Как ты узнал про нас:")
           (with-html-form (:post (lambda (&key value &allow-other-keys)
                                    (setf state :confirm)
                                    (setf (get-how-to-know widget) value)
                                    (update widget)))
             (:input :type "text"
                     :name "value")
             (render-button "Далее")))
         
          ((eql state :confirm)
           (:p "Введённые данные:")
           (:ul
            (:li ("Имя: ~A" (get-name widget)))
            (:li ("Email: ~A" (get-email widget)))
            (:li ("Откуда о нас узнали: ~A" (get-how-to-know widget))))
           (render-form-and-button "Отмена" 
                                   (switch-to-state nil)
                                   :button-class "button warning")
           (render-form-and-button "Всё верно"
                                   (switch-to-state :done)))
          ((eql state :done)
           (:p "Регистрация завершена. Спасибо!")
           (:p "Чтобы вернуться к началу, нажми кнопку:")
           (render-form-and-button "Кнопка"
                                   (switch-to-state nil)))
          (t
           (:p ("Неизвестное состояние: ~A" state))
           (:p "Чтобы вернуться к началу, нажми кнопку:")
           (render-form-and-button "Кнопка"
                                   (switch-to-state nil))))))))


;; (defmethod weblocks/dependencies:get-dependencies ((widget registration))
;;   (list* (weblocks-lass:make-dependency
;;            '(.registration
;;              :width 80%
;;              :margin-left "auto"
;;              :margin-right "auto"))
;;          ;; (weblocks-parenscript:make-dependency
;;          ;;   (alert "Привет!"))
;;          (call-next-method)))


(defun start ()
  (weblocks/server:start :port *port*))

