(in-package #:cl-ring-util)
(annot:enable-annot-syntax)

@export
(defvar default-mime-types
  `(("7z"   . "application/x-7z-compressed")
   ("aac"   . "audio/aac")
   ("ai"    . "application/postscript")
   ("asc"   . "text/plain")
   ("atom"  . "application/atom+xml")
   ("avi"   . "video/x-msvideo")
   ("bin"   . "application/octet-stream")
   ("bmp"   . "image/bmp")
   ("bz2"   . "application/x-bzip")
   ("class" . "application/octet-stream")
   ("cer"   . "application/pkix-cert")
   ("crl"   . "application/pkix-crl")
   ("crt"   . "application/x-x509-ca-cert")
   ("css"   . "text/css")
   ("csv"   . "text/csv")
   ("deb"   . "application/x-deb")
   ("dll"   . "application/octet-stream")
   ("dmg"   . "application/octet-stream")
   ("dms"   . "application/octet-stream")
   ("doc"   . "application/msword")
   ("dvi"   . "application/x-dvi")
   ("eps"   . "application/postscript")
   ("etx"   . "text/x-setext")
   ("exe"   . "application/octet-stream")
   ("flv"   . "video/x-flv")
   ("flac"  . "audio/flac")
   ("gif"   . "image/gif")
   ("gz"    . "application/gzip")
   ("htm"   . "text/html")
   ("html"  . "text/html")
   ("ico"   . "image/x-icon")
   ("iso"   . "application/x-iso9660-image")
   ("jar"   . "application/java-archive")
   ("jpe"   . "image/jpeg")
   ("jpeg"  . "image/jpeg")
   ("jpg"   . "image/jpeg")
   ("js"    . "text/javascript")
   ("json"  . "application/json")
   ("lha"   . "application/octet-stream")
   ("lzh"   . "application/octet-stream")
   ("mov"   . "video/quicktime")
   ("m4v"   . "video/mp4")
   ("mp3"   . "audio/mpeg")
   ("mp4"   . "video/mp4")
   ("mpe"   . "video/mpeg")
   ("mpeg"  . "video/mpeg")
   ("mpg"   . "video/mpeg")
   ("oga"   . "audio/ogg")
   ("ogg"   . "audio/ogg")
   ("ogv"   . "video/ogg")
   ("pbm"   . "image/x-portable-bitmap")
   ("pdf"   . "application/pdf")
   ("pgm"   . "image/x-portable-graymap")
   ("png"   . "image/png")
   ("pnm"   . "image/x-portable-anymap")
   ("ppm"   . "image/x-portable-pixmap")
   ("ppt"   . "application/vnd.ms-powerpoint")
   ("ps"    . "application/postscript")
   ("qt"    . "video/quicktime")
   ("rar"   . "application/x-rar-compressed")
   ("ras"   . "image/x-cmu-raster")
   ("rb"    . "text/plain")
   ("rd"    . "text/plain")
   ("rss"   . "application/rss+xml")
   ("rtf"   . "application/rtf")
   ("sgm"   . "text/sgml")
   ("sgml"  . "text/sgml")
   ("svg"   . "image/svg+xml")
   ("swf"   . "application/x-shockwave-flash")
   ("tar"   . "application/x-tar")
   ("tif"   . "image/tiff")
   ("tiff"  . "image/tiff")
   ("txt"   . "text/plain")
   ("webm"  . "video/webm")
   ("wmv"   . "video/x-ms-wmv")
   ("xbm"   . "image/x-xbitmap")
   ("xls"   . "application/vnd.ms-excel")
   ("xml"   . "text/xml")
   ("xpm"   . "image/x-xpixmap")
   ("xwd"   . "image/x-xwindowdump")
   ("zip"   . "application/zip")))

;TODO use cl-ppre
(defun filename-ext (filename)
  "Returns the file extension of a filename or filepath."
  (cl-ppcre:register-groups-bind (first)
      ("\\.([^./\]+)$" filename)
    first))

@export
(defun ext-mime-type (filename &rest mime-types)
  "Get the mimetype from the filename extension. Takes an optional alist of
  extensions to mimetypes that overrides values in the default-mime-types map."
  (let ((mime-types (merge-alists default-mime-types mime-types)))
    (getval mime-types (filename-ext filename))))


;; (re-find #"\.([^./\\]+)$" filename))
