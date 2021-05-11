(in-package :rest-server)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-documentation-toplevel (api &body body)
    `(cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
       (:html
        (:head
         (:title (who:str (name ,api)))
	 (:link :rel "stylesheet" :href "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" :integrity "sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" :crossorigin "anonymous")
	 (:link :rel "stylesheet" :href "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.61.0/codemirror.min.css" :integrity "sha512-xIf9AdJauwKIVtrVRZ0i4nHP61Ogx9fSRAkCLecmE2dL/U8ioWpDvFCAy4dcfecN72HHB9+7FfQj3aiO68aaaw==" :crossorigin "anonymous")
	 (:script :src "https://code.jquery.com/jquery-3.3.1.slim.min.js" :integrity "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" :crossorigin "anonymous")
	 (:script :src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js" :integrity "sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1" :crossorigin "anonymous")
	 ;; (:script :src "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js" :integrity "sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM" :crossorigin "anonymous")
	 (:script :src "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.61.0/codemirror.min.js" :integrity "sha512-LwxFyHyqmDdcint8dhTSHeJuI+uH5r/vrcGkxH4QhHGP8SpNaS9MVa1BxZRNTb1GPyLBAWrjbWpZTPGgtDgJOw==" :crossorigin "anonymous")

	 (:script :src "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.61.0/addon/edit/matchbrackets.min.js" :integrity "sha512-rjbQerijlYZoHtP8XJGb7xkwnSRUHiydD8NE9fpe01feRM5RDZSZ9Tia+a9hV2cFHMilO4hZaeeTc9ESiik9wQ==" :crossorigin "anonymous")
	 
	 (:script :src "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.61.0/mode/shell/shell.min.js" :integrity "sha512-XgDqDL1gXdiG/pLBnn7W+08Ajap6ycB3jVD0db1oBwtQevc1DIRvU3fotxsr58GvfzH2pi64roId2YkocyAPzQ==" :crossorigin "anonymous")
	 (:style ".CodeMirror { height: 100%; }")
	 )
        (:body
         (:div :class "container-fluid"
	       ,@body)
	 (:script "
var editors = [];

function qsa(sel) {
    return Array.apply(null, document.querySelectorAll(sel));
}

qsa('.example').forEach(function (editorEl) {
    editors.push(CodeMirror.fromTextArea(editorEl, {
	mode: 'shell',
	lineNumbers: true,
	styleActiveLine: true,
	matchBrackets: true,
    }));
});

$(document).ready(function() {

});

$('.nav-pills a').on('shown.bs.tab', function() {
    editors.forEach(function (editor) {
	editor.refresh();
    })
});
")
	 (:script :src "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js" :integrity "sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM" :crossorigin "anonymous"))))))

(defclass api-documentation-acceptor (hunchentoot:acceptor)
  ((api :initarg :api
        :accessor api
        :initform (error "Provide the api")))
  (:documentation "Acceptor for api documentation application"))

(defun start-api-documentation (api address port)
  "Start a web documentation application on the given api."
  (hunchentoot:start
   (make-instance 'api-documentation-acceptor
                  :address address
                  :port port
                  :api (if (symbolp api)
                           (find-api api)
                           api))))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor api-documentation-acceptor) request)
  (loop for resource-operation in (resource-operations (api acceptor))
	for i from 0
     when (equalp (format nil "/~A" (name resource-operation)) (hunchentoot:request-uri*))
     return (with-documentation-toplevel (api acceptor)
              (document-resource-operation resource-operation i))
     finally (return (api-toplevel-documentation acceptor))))

(defun api-toplevel-documentation (acceptor)
  (let ((api (api acceptor)))
    (with-documentation-toplevel api
      (:div :class "row"
      	    (:div :class "col-12"
      		  (:hr :class "my-4")))
      (:div :class "row"
	    (:div :class "col-2"
		  (:div :class "jumbotron"
			(:h1 :class "api-name display-4" :style "text-align: center;" (cl-who:str (name api)))
			(:div :class "api-documentation lead" :style "text-align: center;"
			      (cl-who:str (api-documentation api))))
		  (:div :class "menu nav flex-column nav-pills" :id "menu" :role "tablist" :aria-orientation "vertical"
			(loop for resource-operation in (resource-operations api)
			      for i from 0
			      do (cl-who:htm
				  (:a :class (format nil "nav-link ~a" (if (= i 0) "active" ""))
				      :id (format nil "v-pills-~a-tab" i)
				      :data-toggle "pill"
				      :href (format nil "#v-pills-~a" i)
				      :role "tab"
				      :aria-controls (format nil "v-pills-~a" i)
				      :aria-selected (format nil (if (= i 0) "true" "false"))
				      (cl-who:str (name resource-operation)))))))
	    (:div :class "col-10"
		  (:div :class "resource-operations tab-content" :id "v-pills-tabContent"
			(loop for resource-operation in (resource-operations api)
			      for i from 0
			      do (document-resource-operation resource-operation i)))))
      )))

(defun document-resource-operation (resource-operation index)
  (cl-who:with-html-output (*standard-output*)
    (cl-who:htm
     (:div :class (format nil "resource-operation tab-pane fade ~a"
			  (if (= index 0)
			      "show active"
			      ""))
	   :id (format nil "v-pills-~a" index)
	   :role "tabpanel"
	   :aria-labelledby (format nil "v-pills-tab-~a" index)
           (:div :class "row"
		 (:div :class "col-6"
		       (:div :class "name alert alert-primary"
			     (:h4 :style "text-align: center;"
				  (cl-who:str (name resource-operation))))
		       (:div :class "card"
			     (:div :class "card-body"
				   (:div :class "method btn btn-primary"
					 (cl-who:str (request-method resource-operation)))
				   (:div :class "documentation card-title"
					 (cl-who:str (api-documentation resource-operation)))
				   (:div :class "signature"
					 (:code (cl-who:str (path resource-operation))))))
		       (:div :class "arguments card"
			     (let ((args (required-arguments resource-operation)))
			       (if args
				   (cl-who:htm
				    (:div :class "card-body"
					  (:h6 :class "card-title" "Parameters")
					  (:ul :class "list-group list-group-flush"
					       (loop for arg in args
						     do (cl-who:htm
							 (:li :class "list-group-item"
							      (:div :class "card"
								    (:div :class "card-body"
									  (:div :class "card-title"
										(:code (cl-who:fmt "~a" (rs::argument-name arg))))
									  (:div :class "card-text"
										(cl-who:fmt "~a. ~a"
											    (rs::argument-type-spec (rs::argument-type arg))
											    (argument-documentation arg)))))))))))))
			     (let ((args (optional-arguments resource-operation)))
			       (if args
				   (cl-who:htm
				    (:div :class "card-body"
					  (:h6 :class "card-title" "Optional parameters")
					  (:ul :class "list-group list-group-flush"
					       (loop for arg in args
						     do (cl-who:htm
							 (:li :class "list-group-item"
							      (:div :class "card"
								    (:div :class "card-body"
									  (:div :class "card-title"
										(:code (cl-who:fmt "~a" (rs::argument-name arg))))
									  (:div :class "card-text"
										(cl-who:fmt "~a. ~a. Default: ~a"
											    (rs::argument-type-spec (rs::argument-type arg))
											    (argument-documentation arg)
											    (argument-default arg)))))))))))))))
		 (:div :class "examples col-6"
		       (:textarea :class "example" :id (format nil "example-~a" index)
				  (cl-who:str (file-get-contents (api-examples resource-operation))))))))))

;; API misin that displays a summary documentation of the api for the
;; indicated endpoints (default is root).

(defclass api-docs-mixin ()
  ((docs-path :accessor docs-path :initform "/"))
  (:documentation "Mixin for displaying API documentation at DOC-PATH"))

(defmethod api-dispatch-request :around ((api api-docs-mixin) request)
  (let* ((doc-types (list "text/html" "text/plain"))
         (content-type (mimeparse:best-match doc-types
                                             (hunchentoot:header-in* "accept"))))
    (if (and (member content-type doc-types :test 'string=)
             (ppcre:scan (parse-resource-path (docs-path api))
                         (request-uri request)))
        (print-api-docs api (parse-content-type content-type))
        (call-next-method))))

(defun print-api-docs (api content-type)
  (case content-type
    (:html (print-html-api-docs api))
    (t (print-text-api-docs api))))

(defvar *api-docs-html*)

(defun print-html-api-docs (api)
  (who:with-html-output-to-string (*api-docs-html*)
    (:html
     (:head
      (:title (who:fmt "~a api documentation" (title api)))
      (:style :media "screen" :type "text/css"
              (who:str "table.arguments th, td {
border: 1px solid black;
}")
              (who:str "th {
text-align:left;
}")
              (who:str ".resource {
border: 1px solid lightgray;
background-color: lightblue;
padding: 0px 10px;
margin-bottom: 30px;
box-shadow: 10px 10px 5px gray;
}")
              (who:str ".operation {
border: 1px solid lightgray;
background-color: lightyellow;
padding: 0px 10px;
margin-bottom: 20px;
box-shadow: 5px 5px 5px gray;
}")
              ))
     (:body
      (:h1 (who:str (title api)))
      (:p (who:str (api-documentation api)))
      (:h2 "Resources")
      (loop for resource being the hash-value in (resources api)
         do
           (print-resource-html resource *api-docs-html*))))))

(defun print-resource-html (resource html)
  (who:with-html-output (html)
    (:div :class "resource"
          (:h3 (who:str (resource-name resource)))
          (:p (who:str (resource-path resource)))
          (:p (who:str (resource-documentation resource)))
          (:h4 (who:str "Resource operations"))
          (loop for operation being the hash-value in (resource-operations resource)
             do (print-operation-html operation html)))))

(defun print-operation-html (operation html)
  (who:with-html-output (html)
    (:div :class "operation"
          (:h5 (who:str (name operation)))
          (:p (who:fmt "~a ~a"(request-method operation) (path operation)))
          (:p (who:str (api-documentation operation)))
          (:h6 "Arguments")
          (:table :class "arguments"
                  (:thead
                   (:tr
                    (:th (who:str "Name"))
                    (:th (who:str "Type"))
                    (:th (who:str "Required"))
                    (:th (who:str "Default"))
                    (:th (who:str "Description"))
                    (:tbody
                     (loop for arg in (append (required-arguments operation)
                                              (optional-arguments operation))
                        do (print-argument-html arg html)))))))))

(defun print-argument-html (arg html)
  (who:with-html-output (html)
    (:tr :class "arg"
         (:td (who:str (argument-name arg)))
         (:td (who:str (argument-type-spec (argument-type arg))))
         (:td (who:str (if (argument-required-p arg)
                           "True" "False")))
         (:td (when (argument-default arg)
                (who:fmt "~a" (argument-default arg))))
         (:td (who:str (argument-documentation arg)))
         )))

(defun print-text-api-docs (api)
  "TODO: print api text docs here")
