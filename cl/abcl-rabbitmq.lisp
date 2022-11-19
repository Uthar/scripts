(defpackage rabbit
  (:use :cl)
  (:local-nicknames
   (:j :java)))

(in-package rabbit)

(defmacro def (&body body)
  `(defparameter ,@body))

(def factory (j:jnew "com.rabbitmq.client.ConnectionFactory"))
(def conn (j:jcall "newConnection" factory))
(def chan (j:jcall "createChannel" conn))

(j:jcall "exchangeDeclare" chan "foo" "direct")
(j:jcall "queueDeclare" chan "foo" t nil nil (j:jnew "java.util.HashMap"))
(j:jcall "queueBind" chan "foo" "foo" "")

(defun handler (tag ev props body)
  (format t "got message ~A~%" (j:jnew "java.lang.String" body)))


(j:jnew-runtime-class
 "LispConsumer"
 :superclass "com.rabbitmq.client.DefaultConsumer"
 :constructors '((("com.rabbitmq.client.Channel")
                  (lambda (this channel)
                    (declare (ignore this channel)))
                  (1)))
 :methods '(("handleDelivery" :void
             ("java.lang.String"
              "com.rabbitmq.client.Envelope"
              "com.rabbitmq.client.AMQP$BasicProperties"
              (:array :byte))
             (lambda (this consumer-tag envelope properties body)
               (handler consumer-tag envelope properties body)))))

(def consumer (j:jnew +lisp-consumer+ chan))
(j:jcall "basicConsume" chan "foo" consumer)
(j:jcall "basicPublish" chan "foo" "" j:+null+ (j:jcall "getBytes" "there"))

