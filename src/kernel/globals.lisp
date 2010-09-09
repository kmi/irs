(in-package :irs)

(defvar *host* nil
  "The hostname with which to open the network sockets, thus allowing
multiple IRS servers on one machine.  If HOST is NIL, listen on all
interfaces.")

(defvar *port* 8080
  "The port on which IRS opens its web interface.")

(defvar *connection-read-timeout* 30)

(defvar *connection-timeout* 30)

(defvar *proxy-host* nil)

(defvar *proxy-port* nil)

(defvar *base-href* nil)
