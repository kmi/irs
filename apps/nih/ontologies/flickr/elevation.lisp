;;; Copyright © 2009,2010 The Open University

(in-package #:ocml)

(in-ontology flickr)

;;; {{{ XML formatting

;;; Generic XML processing, not tied to any particulr Flickr service.

(def-rule #_xmllyPhotoList
    ((#_xmllyPhotoList ?photos-element ?photolist) if
     (= ?ps
	(setofall ?photo
		  (and (#_xml:elementByName ?photos-element ?photoel "photo")
		       (#_xmllyPhoto ?photo ?photoel))))
     (= ?ps ?photolist)))

(def-rule #_xmllyPhoto
    ((#_xmllyPhoto ?photo ?xml) if
     (#_xml:Element ?xml)
     (= ?pairs (setofall (?name ?value) (#_xml:attributeNameValue ?xml ?name ?value)))
     (#_xml:attributeNameValue ?xml "title" ?title)
     (#_xml:attributeNameValue ?xml "id" ?id)
     (#_xml:attributeNameValue ?xml "farm" ?farm)
     (#_xml:attributeNameValue ?xml "server" ?server)
     (#_xml:attributeNameValue ?xml "secret" ?secret)
     (= ?slots ((#_title ?title) (#_id ?id) (#_farm ?farm)
		(#_server ?server) (#_secret ?secret)))
     (#_new ?photo #_Photo ?slots)))

(def-rule #_xmllyPhotoSize
    ((#_xmllyPhotoSize ?photosize ?xml) if
     (#_xml:Element ?xml)
     (#_xml:tag ?xml "size")
     (= ?pairs (setofall (?name ?value) (#_xml:attributeNameValue ?xml ?name ?value)))
     (#_xml:attributeNameValue ?xml "label" ?label)
     (#_xml:attributeNameValue ?xml "width" ?width)
     (#_xml:attributeNameValue ?xml "height" ?height)
     (#_xml:attributeNameValue ?xml "source" ?source)
     (= ?slots ((#_label ?label) (#_width ?width) (#_height ?height)
		(#_source ?source)))
     (#_new ?photosize #_PhotoSize ?slots)))

(def-rule #_xmllyPhotoSizeList
    ((#_xmllyPhotoSizeList ?sizes-element ?photosizelist) if
     (= ?ps
	(setofall ?size
		  (and (#_xml:elementByName ?sizes-element ?sizeel "size")
		       (#_xmllyPhotoSize ?size ?sizeel))))
     (= ?ps ?photosizelist)))

;;; }}}
