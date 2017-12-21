(ns potato.fineuploader
    (:require [goog.dom]
              [goog.style]
              [goog.dom.classes]))

(def uploading-text     "Uploading")
(def nonbreak-space     "\u00a0")

(def s3-signature-url           "/s3/signature")
(def s3-upload-success-url      "/s3/success")
(def s3-create-key-url          "/s3/create_key")
(def s3-success-page-url        "/success.html")

(defn fine-uploader-promise [instance file-id current-channel-id]
  (let [key-retrieval (new js/qq.Promise)
        filename      (.encodeURIComponent js/window (.getName (instance) file-id))
        xhr           (new js/XMLHttpRequest)]
    (set! (.-onreadystatechange xhr) #(if (== (.-readyState xhr) 4)
                                        (if (not (== (.-status xhr) 200))
                                          (.failure key-retrieval)
                                          (.success key-retrieval (.-responseText xhr)))))
    (.open xhr "POST" (str s3-create-key-url "?filename=" filename "&channel=" current-channel-id "&message="))
    (.setRequestHeader xhr "Content-Type" "application/json")
    (.send xhr)
    key-retrieval))

(defn fine-uploader-on-progress-callback [id name uploaded-bytes total-bytes]
  (goog.dom.classes/add (goog.dom/getElement "upload-progress") "active")
  (let [percent-done (/ (* 100 uploaded-bytes) total-bytes)]
    (goog.dom/setTextContent (goog.dom/getElement "uploading-text") (str uploading-text nonbreak-space name "…"))
    (goog.dom/setTextContent (goog.dom/getElement "uploading-percent") (str (Math/floor percent-done) "%"))
    (goog.style/setStyle (goog.dom/getElement "uploading-percent") "left" (str percent-done "%"))))

(defn fine-uploader-on-complete-callback [response success xhr-or-xdr]
  (goog.dom.classes/remove (goog.dom/getElement "upload-progress") "active"))

(defn create-fine-uploader-s3 [instance-callback current-user-id current-channel-id]
  ;; Workaround for a bug in fineuploader that prevents it from using
  ;; the "bucket" key in the "request" struct.
  (set! js/qq.s3.util.getBucket (fn [_] (:bucket potato.state/init-s3-credentials)))
  ;;
  (new js/qq.s3.FineUploaderBasic
       (clj->js {"request"          {"endpoint"  (:endpoint potato.state/init-s3-credentials)
                                     "bucket"    (:bucket potato.state/init-s3-credentials)
                                     "accessKey" (:accessKey potato.state/init-s3-credentials)
                                     "params"    {"channel" current-channel-id
                                                  "user"    current-user-id}}
                 "objectProperties" {"key" #(fine-uploader-promise instance-callback % current-channel-id)}
                 "signature"        {"endpoint" s3-signature-url}
                 "uploadSuccess"    {"endpoint" s3-upload-success-url}
                 "callbacks"        {"onProgress" fine-uploader-on-progress-callback
                                     "onComplete" fine-uploader-on-complete-callback}
                 "iframeSupport"    {"localBlankPagePath" s3-success-page-url}})))

(defn create-fine-uploader-local [instance-callback current-user-id current-channel-id]
  (new js/qq.FineUploaderBasic
       (clj->js {"request"          {"endpoint"  "/upload"
                                     "params" {"channel" current-channel-id
                                               "user"    current-user-id}}
                 "objectProperties" {"key" #(fine-uploader-promise instance-callback % current-channel-id)}
                 "callbacks"        {"onProgress" fine-uploader-on-progress-callback
                                     "onComplete" fine-uploader-on-complete-callback}
                 "iframeSupport"    {"localBlankPagePath" s3-success-page-url}})))

(defn create-fine-uploader [instance-callback current-user-id current-channel-id]
  (if potato.state/init-s3-credentials
    (create-fine-uploader-s3 instance-callback current-user-id current-channel-id)
    (create-fine-uploader-local instance-callback current-user-id current-channel-id)))

(defn enable-drag-and-drop [fine-uploader-instance callback]
  (new js/qq.DragAndDrop
       (clj->js {"dropZoneElements" [ (goog.dom/getElement "potato-root") (goog.dom/getElement "help")]
                 "classes" {"dropActive" "drop-is-active"}
                 "callbacks" {"processingDroppedFiles" #(println "Processing...")
                              "processingDroppedFilesComplete" (fn [files target]
                                                                 (.addFiles fine-uploader-instance files)
                                                                 (callback))}})))
