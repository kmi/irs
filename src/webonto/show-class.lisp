;;; Mode: Lisp; Package: http

;;; Author: Tim Gamble

;;; The Open University

(in-package "HTTP")


;;; This function displays three columns using HTML to show a class, all of the slots, all of the slots for the types, and instances for the first column of slots: 

(define-page ("Show OCML Class" :func-name show-class :bgcolor "white" :link "green" :vlink "green" :text "blue")
    (&optional (ontology "observatory-kb") (class "learning-initiative") (back "()"))
  (setq ontology (read-from-string (concatenate 'string "ocml::" ontology)))
  (setq class (read-from-string (concatenate 'string "ocml::" class)))
  (setq back (read-from-string back))
  (ocml::select-ontology ontology)
(if (equal back ())
      (html:html-out "~%<P><FONT COLOR=ORANGE>(You are at the top level. Use the \"*\" to view all instances. Use the \"+\" to view all subclasses.)</FONT></P>")
    (html:html-out "~%<P><FONT COLOR=ORANGE>(Click here to go back to: <A HREF=\"/show-class/~(~a~)/~(~a~)/~(~a~)\">~(~a~)</A>. Use the \"*\" to view all instances. Use the \"+\" to view all subclasses.)</FONT></P>" ontology (first back) (cdr back) (first back)))
  (html:html-out "<TABLE BORDER=0>~%")
  (dolist (slot (web-onto::sorted-slot-names class))
    (html:html-out "<TR>~%<TD valign=top>~%<BR><B><A HREF=\"ocml-definition?ocml-name=~a?ocml-ontology=~a\"><FONT COLOR=BLUE><I>~(~a~)</I></FONT></A></B><A HREF=\"/show-all-classes/~(~a~)/~(~a~)/~(~a~)/~(~a~)\">+</A></TD>~%</TR>~%" slot ontology slot ontology class slot (cons class back))
    (dolist (type (http::slot-types (ocml::assoc slot (ocml::slot-info-alist (ocml::get-domain-class class)))))
      (if (eq 0 (length (web-onto::sorted-slot-names type)))
          (html:html-out "<TR>~%<TD valign=top>~%<A HREF=\"ocml-definition?ocml-name=~a?ocml-ontology=~a\"><FONT COLOR=GREEN>~(~a~)</FONT></A><A HREF=\"/show-instance/~(~a~)/~(~a~)/~(~a~)\">*</A></TD>~%<TD valign=top>~%" type ontology type ontology type (cons class back))
        (html:html-out "<TR>~%<TD valign=top>~%<A HREF=\"/show-class/~(~a~)/~(~a~)/~(~a~)\">~(~a~)</A>~%<A HREF=\"/show-instance/~(~a~)/~(~a~)/~(~a~)\">*</A></TD>~%<TD valign=top>~%" ontology type (cons class back) type ontology type (cons class back)))
      (dolist (subslot (web-onto::sorted-slot-names type))
        (html:html-out "<B><A HREF=\"ocml-definition?ocml-name=~a?ocml-ontology=~a\"><FONT COLOR=BLUE><I>~(~a~)</I></FONT></B><A HREF=\"/show-all-classes/~(~a~)/~(~a~)/~(~a~)/~(~a~)\">+</A><BR>~%" subslot ontology subslot ontology type subslot (cons class back))
        (dolist (subtype (http::slot-types (ocml::assoc subslot (ocml::slot-info-alist (ocml::get-domain-class type)))))
          (if (eq 0 (length (web-onto::sorted-slot-names subtype)))
              (html:html-out "<A HREF=\"ocml-definition?ocml-name=~a?ocml-ontology=~a\"><FONT COLOR=GREEN>~(~a~)</FONT></A><A HREF=\"/show-instance/~(~a~)/~(~a~)/~(~a~)\">*</A><BR><BR>~%" subtype ontology subtype ontology subtype (cons class back))
            (html:html-out "<A HREF=\"/show-class/~(~a~)/~(~a~)/~(~a~)\">~(~a~)</A>~%<A HREF=\"/show-instance/~(~a~)/~(~a~)/~(~a~)\">*</A><BR>~%" ontology subtype (cons class back) subtype ontology subtype (cons class back)))))
      (html:html-out "</TD>~%<TD valign=top>~%")
      (if (eq (ocml::careful-all-instances-from-type type) nil) (html:html-out "<FONT COLOR=PURPLE>(None)</FONT>"))
      (dolist (y (mapcar #'ocml::name (ocml::careful-all-instances-from-type type)))
        (html:html-out "<A HREF=\"ocml-definition?ocml-name=~a?ocml-ontology=~a\"><FONT COLOR=PURPLE>~(~a~)</FONT></A><br>~%" y ontology y))
      (html:html-out "</TD>~%</TR>~%")))
  (html:html-out "<CAPTION><B><A HREF=\"ocml-definition?ocml-name=~a?ocml-ontology=~a\"><FONT COLOR=RED>~(~a~)</FONT></A></B><A HREF=\"/show-instance/~(~a~)/~(~a~)/~(~a~)\">*</A></CAPTION>~%</TABLE>" class ontology class ontology class (cons class back)))

;;; Displays all instances associated with a particular class:

(define-page ("Show OCML Instance" :func-name show-instance :bgcolor "white" :link "green" :vlink "green" :text "blue")
    (&optional (ontology "observatory-kb") (class "learning-initiative") (back "()"))
  (setq ontology (read-from-string (concatenate 'string "ocml::" ontology)))
  (setq class (read-from-string (concatenate 'string "ocml::" class)))
  (setq back (read-from-string back))
  (ocml::select-ontology ontology)
  (if (equal back ())
      (html:html-out "~%<P><FONT COLOR=ORANGE>(Nowhere to go back to. Use the \"*\" to view all instances. Use the \"+\" to view all subclasses.)</FONT></P>")
    (html:html-out "~%<P><FONT COLOR=ORANGE>(Click here to go back to: <A HREF=\"/show-class/~(~a~)/~(~a~)/~(~a~)\">~(~a~)</A>. Use the \"*\" to view all instances. Use the \"+\" to view all subclasses.)</FONT></P>" ontology (first back) (cdr back) (first back)))
  (html:html-out "~%<TABLE BORDER=0>~%")
  (html:html-out "<TR>~%<TD valign=top><B><A HREF=\"ocml-definition?ocml-name=~a?ocml-ontology=~a\"><FONT COLOR=GREEN>~(~a~)</FONT></A><B></TD>~%<TD>~%" class ontology class)
  (if (eq (ocml::careful-all-instances-from-type class) nil) (html:html-out "(None)</TD>~%</TR>~%"))
  (dolist (y (mapcar #'ocml::name (ocml::careful-all-instances-from-type class)))
    (html:html-out "<A HREF=\"ocml-definition?ocml-name=~a?ocml-ontology=~a\"><FONT COLOR=PURPLE>~(~a~)</FONT></A><br>~%" y ontology y))
  (html:html-out "</TD>~%</TR>~%")
  (html:html-out "</TABLE>"))

;;;This shows all the classes (not just super classes) for a particular slot:

(define-page ("Show OCML Classes from Type" :func-name show-all-classes :bgcolor "white" :link "green" :vlink "green" :text "blue")
    (&optional (ontology "observatory-kb") (class "learning-initiative") (slot "has-funder") (back "()"))
  (setq ontology (read-from-string (concatenate 'string "ocml::" ontology)))
  (setq class (read-from-string (concatenate 'string "ocml::" class)))
  (setq slot (read-from-string (concatenate 'string "ocml::" slot)))
  (setq back (read-from-string back))
  (ocml::select-ontology ontology)
  (if (equal back ())
      (html:html-out "~%<P><FONT COLOR=ORANGE>(Nowhere to go back to. Use the \"*\" to view all instances. Use the \"+\" to view all subclasses.)</FONT></P>")
   (html:html-out "~%<P><FONT COLOR=ORANGE>(Click here to go back to: <A HREF=\"/show-class/~(~a~)/~(~a~)/~(~a~)\">~(~a~)</A>. Use the \"*\" to view all instances. Use the \"+\" to view all subclasses.)</FONT></P>" ontology (first back) (cdr back) (first back)))
  (html:html-out "~%<TABLE BORDER=0>~%")
  (html:html-out "<TR>~%<TD valign=top><B><A HREF=\"ocml-definition?ocml-name=~a?ocml-ontology=~a\"><FONT COLOR=BLUE>~(~a~)</FONT></A><B></TD>~%<TD>~%<FONT COLOR=GREEN>~%" slot ontology slot)
  (dolist (y (web-onto::index-aspect-types class slot))
    (if (eq 0 (length (web-onto::sorted-slot-names y)))
        (html:html-out "<A HREF=\"ocml-definition?ocml-name=~a?ocml-ontology=~a\">~(~a~)</A><A HREF=\"/show-instance/~(~a~)/~(~a~)/~(~a~)\">*</A><br>~%" y ontology y ontology y back)
      (html:html-out "<A HREF=\"/show-class/~(~a~)/~(~a~)/~(~a~)\">~(~a~)</A><A HREF=\"/show-instance/~(~a~)/~(~a~)/~(~a~)\">*</A><br>~%" ontology y back y ontology y back)))
    (html:html-out "</FONT>~%</TD>~%</TR>~%")
  (html:html-out "</TABLE>"))


(defun slot-types (single-slot-info-alist)
  (let ((type-names (ocml::ocml-slot-type (cdr single-slot-info-alist))))
    (remove-duplicates
      (mapcan #'(lambda (type-name)
                  (if (listp type-name)
                      ;;either or or and
                      (copy-list (cdr type-name))
                      (list type-name)))
              type-names))))
