;; Initial version based on 
;;    https://github.com/alexott/emacs-configs/blob/master/rc/emacs-rc-cedet.el
;;
;; See also Alex's article
;;    http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
;;
;; 
;; DEBUGGING:
;;   For debugging of semantic do:
;;     1. Load the library: M-x load-library semantic/analyze/debug
;;     2. Turn on debugging for semantic: M-x semantic-analyze-debug-assit
;;
;;


;; The cedet bzr version is stored in the .emacs.d/cedet-bzr. If you don't want 
;; to use the bazaar version of cedet, but the build in version which comes with
;; emacs comment out the following few lines.
(load-file "~/.emacs.d/cedet-bzr/cedet/cedet-devel-load.el")
(load-file "~/.emacs.d/cedet-bzr/cedet/contrib/cedet-contrib-load.el")
(add-to-list 'load-path "~/.emacs.d/cedet-bzr/cedet/contrib/")
(add-to-list 'Info-directory-list "~/.emacs.d/cedet-bzr/cedet/doc/info")


;;;;;;;;;;;;;;;;;;;;;;;
;; CEDET minor modes ;;
;;;;;;;;;;;;;;;;;;;;;;;
;; Enables global support for Semanticdb
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)

;; Enables automatic bookmarking of tags that you edited, so you can return to 
;; them later with the semantic-mrub-switch-tags (M-x B) command, e.g. after a
;; jump (semantic-ia-fast-jump)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)

;; activates CEDET's context menu that is bound to right mouse button
;;(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)

;; Activates automatic parsing of source code in the idle time
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)


;; Activates displaying of possible name completions in the idle time. Requires
;;  that global-semantic-idle-scheduler-mode was enabled;
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)

;; Activates displaying of information about current tag in the idle time. 
;; Requires that global-semantic-idle-scheduler-mode was enabled.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)


;; Activates mode when name of current tag will be shown in top line of buffer;
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)

;; Activates highlighting of first line for current tag (function, class, 
;; etc.);
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)

;; Activates use of separate styles for tags decoration (depending on tag's
;; class). These styles are defined in the semantic-decoration-styles list.
;(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)

;; Activates highlighting of local names that are the same as name of tag under
;; cursor.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CEDET minor modes useful for debugging ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(add-to-list 'semantic-default-submodes 'global-semantic-show-unmatched-syntax-mode)
;;(add-to-list 'semantic-default-submodes 'global-semantic-highlight-edits-mode)
;;(add-to-list 'semantic-default-submodes 'global-semantic-show-parser-state-mode)



;;;;;;;;;;;;;;;;;;;;;;;
;; ACTIVATE PACKAGES ;;
;;;;;;;;;;;;;;;;;;;;;;;
;; Activate semantic
(semantic-mode 1)

(require 'semantic/bovine/c)
(require 'semantic/bovine/clang)

(require 'cedet-files)
(require 'srecode)

;; loading contrib packages
(require 'eassist)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOMISATION OF MODES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun alexott/cedet-hook ()
  (local-set-key [(control return)]  'semantic-ia-complete-symbol)
  (local-set-key "\C-c?"             'semantic-ia-complete-symbol-menu)
  ;;
  (local-set-key "\C-c>"             'semantic-complete-analyze-inline)
  (local-set-key "\C-c="             'semantic-decoration-include-visit)

  ;; Use C-x B to jump back
  (local-set-key "\C-cj"             'semantic-ia-fast-jump)
  (local-set-key "\C-cq"             'semantic-ia-show-doc)
  (local-set-key "\C-cs"             'semantic-ia-show-summary)
  (local-set-key "\C-cp"             'semantic-analyze-proto-impl-toggle)
;; (local-set-key (kbd "C-c <left>") 'semantic-tag-folding-fold-block)
;; (local-set-key (kbd "C-c <right>") 'semantic-tag-folding-show-block)

  ;**; (add-to-list 'ac-sources 'ac-source-semantic)
  )
;; (add-hook 'semantic-init-hooks 'alexott/cedet-hook)
(add-hook 'c-mode-common-hook   'alexott/cedet-hook)
(add-hook 'lisp-mode-hook       'alexott/cedet-hook)
(add-hook 'scheme-mode-hook     'alexott/cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'alexott/cedet-hook)
(add-hook 'erlang-mode-hook     'alexott/cedet-hook)

(defun alexott/c-mode-cedet-hook ()
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-xt" 'eassist-switch-h-cpp)
  (local-set-key "\C-ce" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref)

  ;**; (add-to-list 'ac-sources 'ac-source-gtags)
  )
(add-hook 'c-mode-common-hook 'alexott/c-mode-cedet-hook)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOMIZATION OF SEMANTIC DB ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;**; (when (cedet-gnu-global-version-check t)
;**;   (semanticdb-enable-gnu-global-databases 'c-mode t)
;**;   (semanticdb-enable-gnu-global-databases 'c++-mode t))

;**; (when (cedet-ectag-version-check t)
;**;   (semantic-load-enable-primary-ectags-support))

;; SRecode
(global-srecode-minor-mode 1)

;; Add semantics to imenus
(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)



;;;;;;;;;
;; EDE ;;
;;;;;;;;;
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 

;; Use ede-new to create a new project



;; helper for boost setup...
(defun c++-setup-boost (boost-root)
  (when (file-accessible-directory-p boost-root)
    (let ((cfiles (cedet-files-list-recursively boost-root "\\(config\\|user\\)\\.hpp")))
      (dolist (file cfiles)
        (add-to-list 'semantic-lex-c-preprocessor-symbol-file file)))))


;; my functions for EDE
(defun alexott/ede-get-local-var (fname var)
  "fetch given variable var from :local-variables of project of file fname"
  (let* ((current-dir (file-name-directory fname))
         (prj (ede-current-project current-dir)))
    (when prj
      (let* ((ov (oref prj local-variables))
            (lst (assoc var ov)))
        (when lst
          (cdr lst))))))

;; setup compile package
(require 'compile)
(setq compilation-disable-input nil)
(setq compilation-scroll-output t)
(setq mode-compile-always-save-buffer-p t)

(defun alexott/compile ()
  "Saves all unsaved buffers, and runs 'compile'."
  (interactive)
  (save-some-buffers t)
  (let* ((fname (or (buffer-file-name (current-buffer)) default-directory))
(current-dir (file-name-directory fname))
         (prj (ede-current-project current-dir)))
    (if prj
(project-compile-project prj)
(compile compile-command))))
(global-set-key [f9] 'alexott/compile)

;;
(defun alexott/gen-std-compile-string ()
  "Generates compile string for compiling CMake project in debug mode"
  (let* ((current-dir (file-name-directory
                       (or (buffer-file-name (current-buffer)) default-directory)))
         (prj (ede-current-project current-dir))
         (root-dir (ede-project-root-directory prj)))
    (concat "cd " root-dir "; make -j2")))

;;
(defun alexott/gen-cmake-debug-compile-string ()
  "Generates compile string for compiling CMake project in debug mode"
  (let* ((current-dir (file-name-directory
                       (or (buffer-file-name (current-buffer)) default-directory)))
         (prj (ede-current-project current-dir))
         (root-dir (ede-project-root-directory prj))
         (subdir "")
         )
    (when (string-match root-dir current-dir)
      (setf subdir (substring current-dir (match-end 0))))
    (concat "cd " root-dir "Debug/" "; make -j3")))



;;;;;;;;;;;;;;;;;;
;; Autocomplete ;;
;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-20150618.1949")
(add-to-list 'load-path "~/.emacs.d/elpa/popup-20150626.711")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (expand-file-name
             "~/.emacs.d/elpa/auto-complete-20150618.1949/dict"))
(setq ac-comphist-file (expand-file-name
             "~/.emacs.d/ac-comphist.dat"))
(ac-config-default)


;;; Projects
;;
;; EXAMPLES
;;
;; cpp-tests project definition
;; (when (file-exists-p "~/projects/lang-exp/cpp/CMakeLists.txt")
;;   (setq cpp-tests-project
;; 	(ede-cpp-root-project "cpp-tests"
;; 			      :file "~/projects/lang-exp/cpp/CMakeLists.txt"
;; 			      :system-include-path '("/home/ott/exp/include"
;; 						     boost-base-directory)
;; 			      :compile-command "cd Debug && make -j2"
;; 			      )))
;;
;; (when (file-exists-p "~/projects/squid-gsb/README")
;;   (setq squid-gsb-project
;; 	(ede-cpp-root-project "squid-gsb"
;; 			      :file "~/projects/squid-gsb/README"
;; 			      :system-include-path '("/home/ott/exp/include"
;; 						     boost-base-directory)
;; 			      :compile-command "cd Debug && make -j2"
;; 			      )))
;;

;; GalileoNG runtime progject
(ede-cpp-root-project "galileo_runtime"
		      :file "~/Eaton/GalileoNG/GalNG_4/runtime/CMakeLists.txt"
		      :system-include-path '("/usr/include"
					     "/usr/include/c++/4.9"
					     boost-base-directory
					     )
		      :include-path '(
				      "/communicationJumper"
				      "/communicationJumper/Testing"
				      "/Utility"
				      "/Utility/Memory"
				      "/Utility/Types"
				      "/Utility/Thread"
				      "/Utility/Exception"
				      "/VS"
				      "/external"
				      "/external/boost"
				      "/external/boost/include"
				      "/external/boost/lib"
				      "/external/boost/lib/msvc-9.0"
				      "/external/boost/lib/msvc-9.0/Win32"
				      "/external/TES"
				      "/external/TES/Guiliani"
				      "/external/TES/Guiliani/Guiliani_Library_Eaton"
				      "/external/TES/Guiliani/Guiliani_Library_Eaton/share"
				      "/external/TES/Guiliani/Guiliani_Library_Eaton/include"
				      "/external/TES/Guiliani/Guiliani_Library_Eaton/include/core"
				      "/external/TES/Guiliani/Guiliani_Library_Eaton/include/guiconfig"
				      "/external/TES/Guiliani/Guiliani_Library_Eaton/include/image_decoder"
				      "/external/TES/Guiliani/Guiliani_Library_Eaton/include/platform"
				      "/external/TES/Guiliani/Guiliani_Library_Eaton/include/platform/eGML"
				      "/external/TES/Guiliani/Guiliani_Library_Eaton/include/platform/general"
				      "/external/TES/Guiliani/Guiliani_Library_Eaton/include/platform/GL"
				      "/external/TES/Guiliani/Guiliani_Library_Eaton/include/platform/GL/control"
				      "/external/TES/Guiliani/Guiliani_Library_Eaton/include/platform/GL/EAGL"
				      "/external/TES/Guiliani/Guiliani_Library_Eaton/include/platform/GL/OGLES"
				      "/external/TES/Guiliani/Guiliani_Library_Eaton/include/platform/GL/scene_control"
				      "/external/TES/Guiliani/Guiliani_Library_Eaton/include/platform/GL/OGL"
				      "/external/TES/Guiliani/Guiliani_Library_Eaton/include/platform/win"
				      "/external/TES/Guiliani/Guiliani_Library_Eaton/include/platform/win/pc"
				      "/external/TES/Guiliani/Guiliani_Library_Eaton/include/platform/win/pocket_pc"
				      "/external/TES/Guiliani/Guiliani_Library_Eaton/include/platform/win/smartphone"
				      "/external/TES/Guiliani/Guiliani_Library_Eaton/include/platform/win/OGL"
				      "/external/TES/eGML_Eaton_ProductionLib/comps"
				      "/external/TES/eGML_Eaton_ProductionLib/comps/eGML"
				      "/external/TES/eGML_Eaton_ProductionLib/comps/eGML/code"
				      "/external/TES/eGML_Eaton_ProductionLib/comps/eGML/code/eGML_Pocket"
				      "/external/TES/eGML_Eaton_ProductionLib/comps/eGML/code/eGML_GDI"
				      "/external/TES/eGML_Eaton_ProductionLib/comps/eDBG"
				      "/external/TES/eGML_Eaton_ProductionLib/comps/eDBG/code"
				      "/external/TES/eGML_Eaton_ProductionLib/comps/eFnt"
				      "/external/TES/eGML_Eaton_ProductionLib/comps/eFnt/code"
				      "/external/TES/eGML_Eaton_ProductionLib/comps/eFnt/fonts"
				      "/external/TES/eGML_Eaton_ProductionLib/comps/eImg"
				      "/external/TES/eGML_Eaton_ProductionLib/comps/eImg/code"
				      "/external/TES/eGML_Eaton_ProductionLib/comps/eImg/code/eImg_Tga"
				      "/external/TES/eGML_Eaton_ProductionLib/comps/eImg/code/eImg_Gif"
				      "/external/TES/eGML_Eaton_ProductionLib/comps/eImg/code/eImg_Bmp"
				      "/external/TES/eGML_Eaton_ProductionLib/comps/eImg/code/eImg_Png"
				      "/external/TES/eGML_Eaton_ProductionLib/comps/eImg/code/eImg_Jpeg"
				      "/external/TES/eGML_Eaton_ProductionLib/comps/eMem"
				      "/external/TES/eGML_Eaton_ProductionLib/comps/eMem/code"
				      "/external/TES/eGML_Eaton_ProductionLib/comps/eC"
				      "/external/TES/eGML_Eaton_ProductionLib/comps/eC/code"
				      "/external/TES/eGML_Eaton_ProductionLib/comps/eC/code/pocket"
				      "/external/TES/eGML_Eaton_ProductionLib/comps/eC/code/x86_win32"
				      "/external/TES/ZLib"
				      "/external/TES/ZLib/incluse"
				      "/external/TES/eC"
				      "/external/TES/eC/include"
				      "/external/TES/eC/include/pocket"
				      "/external/TES/eC/include/x86_win32"
				      "/external/TES/LibPng"
				      "/external/TES/LibPng/include"
				      "/external/TES/FreeType-2.4.8"
				      "/external/TES/FreeType-2.4.8/include"
				      "/external/TES/FreeType-2.4.8/include/freetype"
				      "/external/TES/FreeType-2.4.8/include/freetype/internal"
				      "/external/TES/FreeType-2.4.8/include/freetype/internal/services"
				      "/external/TES/FreeType-2.4.8/include/freetype/config"
				      "/external/OS"
				      "/external/OS/interface"
				      "/external/OS/idl"
				      "/external/OS/component"
				      "/external/OS/vc"
				      "/external/CoDeSys2"
				      "/external/CoDeSys2/include"
				      "/external/CoDeSys2/lib"
				      "/external/OpenGL"
				      "/external/OpenGL/include"
				      "/external/OpenGL/include/GL"
				      "/external/CoDeSys3"
				      "/external/CoDeSys3/PlcHandler"
				      "/external/CoDeSys3/PlcHandler/Include"
				      "/external/CoDeSys3/PlcHandler/Include/Windows"
				      "/CMakeScripts"
				      "/dataModel"
				      "/runner"
				      "/runner/src"
				      "/runner/src/specialfunctions"
				      "/runner/src/tags"
				      "/runner/src/tags/Value"
				      "/runner/src/nt"
				      "/runner/src/guiconfig"
				      "/runner/src/graph"
				      "/runner/src/controls"
				      "/runner/src/renderer"
				      "/runner/src/print"
				      "/runner/src/print/NT"
				      "/runner/src/print/CE"
				      "/runner/src/script"
				      "/runner/src/access"
				      "/runner/src/access/nt"
				      "/runner/src/access/ce"
				      "/runner/src/irrLicht"
				      "/runner/src/recipe"
				      "/runner/src/ce"
				      "/runner/res"
				      "/communication"
				      "/communication/3rd_party"
				      "/communication/3rd_party/BC3"
				      "/communication/3rd_party/ADS"
				      "/communication/3rd_party/OPC"
				      "/communication/Testing"
				      "/communication/Testing/UnitTests"
				      "/communication/Testing/UnitTests/CommHandler"
				      "/communication/Testing/UnitTests/ThcSvrs"
				      "/communication/Testing/UnitTests/ThcSvrs/ABMicroLogix"
				      "/communication/Testing/UnitTests/ThcSvrs/ABMicroLogix/Protocol"
				      "/communication/Testing/UnitTests/ThcSvrs/ABMicroLogix/Optimization"
				      "/communication/Testing/UnitTests/ThcSvrs/ABMicroLogix/Helpers"
				      "/communication/Testing/UnitTests/GCUtil"
				      "/communication/Testing/UnitTests/GCUtil/Container"
				      "/communication/Testing/UnitTests/GCUtil/Container/TS"
				      "/communication/Testing/UnitTests/GCUtil/String"
				      "/communication/Testing/UnitTests/additional_input_data"
				      "/communication/Testing/UnitTests/additional_input_data/MAC"
				      "/communication/Testing/UnitTests/Scheduler"
				      "/communication/Testing/UnitTests/runner"
				      "/communication/Testing/UnitTests/CommProtocols"
				      "/communication/Testing/UnitTests/CommProtocols/Ethernet"
				      "/communication/Testing/UnitTests/CommProtocols/Ethernet/IP"
				      "/communication/Testing/CodeSys2Test"
				      "/communication/Testing/CommManagerTest"
				      "/communication/Testing/CodeSys3PlcHandlerTest"
				      "/communication/Testing/CommunicationTest"
				      "/communication/Testing/CommunicationTest/TestCases"
				      "/communication/Testing/CommunicationTest/TestCases/Private"
				      "/communication/Evaluations"
				      "/communication/Evaluations/E0003"
				      "/communication/Evaluations/E0003/src"
				      "/communication/Evaluations/E0001"
				      "/communication/Evaluations/E0001/result"
				      "/communication/Evaluations/E0001/result/2.WithNagleAndZeroBuffer"
				      "/communication/Evaluations/E0001/result/2.WithNagleAndZeroBuffer/3.ZeroSendBuffer"
				      "/communication/Evaluations/E0001/result/2.WithNagleAndZeroBuffer/2.NagleOff"
				      "/communication/Evaluations/E0001/result/2.WithNagleAndZeroBuffer/1.Base"
				      "/communication/Evaluations/E0001/result/1.Eval"
				      "/communication/Evaluations/E0001/src"
				      "/communication/Evaluations/E0001/display"
				      "/communication/Evaluations/E0002"
				      "/communication/Evaluations/E0002/src"
				      "/communication/GC"
				      "/communication/GC/CommHandler"
				      "/communication/GC/CommHandler/Tag"
				      "/communication/GC/CommHandler/Tag/Special"
				      "/communication/GC/CommHandler/Tag/Special/detail"
				      "/communication/GC/CommHandler/Command"
				      "/communication/GC/CommHandler/TagVisitor"
				      "/communication/GC/CommDrivers"
				      "/communication/GC/CommDrivers/SiemensS7"
				      "/communication/GC/CommDrivers/SiemensS7/MPI"
				      "/communication/GC/CommDrivers/SiemensS7/ProfibusStandardProfile"
				      "/communication/GC/CommDrivers/SiemensS7/PPI"
				      "/communication/GC/CommDrivers/SiemensS7/Protocol"
				      "/communication/GC/CommDrivers/SiemensS7/Ethernet"
				      "/communication/GC/CommDrivers/ABMicroLogixENIP"
				      "/communication/GC/CommDrivers/CoDeSys2"
				      "/communication/GC/CommDrivers/CoDeSysPlcHandler"
				      "/communication/GC/ThcSvrs"
				      "/communication/GC/ThcSvrs/thc"
				      "/communication/GC/ThcSvrs/thc/idl"
				      "/communication/GC/ThcSvrs/thc/src"
				      "/communication/GC/ThcSvrs/thc/src/thc"
				      "/communication/GC/ThcSvrs/DllSupport"
				      "/communication/GC/ThcSvrs/BeckhoffADS"
				      "/communication/GC/ThcSvrs/BeckhoffADS/Symbolic"
				      "/communication/GC/ThcSvrs/BeckhoffADS/Image"
				      "/communication/GC/ThcSvrs/ABSlc"
				      "/communication/GC/ThcSvrs/CoDeSys2_NEW"
				      "/communication/GC/ThcSvrs/ABMicroLogix"
				      "/communication/GC/ThcSvrs/ABMicroLogix/Utility"
				      "/communication/GC/ThcSvrs/ABMicroLogix/Protocol"
				      "/communication/GC/ThcSvrs/ABMicroLogix/Optimization"
				      "/communication/GC/ThcSvrs/ABMicroLogix/Helpers"
				      "/communication/GC/ThcSvrs/CoDeSys2"
				      "/communication/GC/ThcSvrs/ThcSvrLib"
				      "/communication/GC/ThcSvrs/CoDeSysPlcHandler"
				      "/communication/GC/ThcSvrs/CoDeSysPlcHandler/Thc"
				      "/communication/GC/ThcSvrs/CoDeSysPlcHandler/res"
				      "/communication/GC/GCUtil"
				      "/communication/GC/GCUtil/Chrono"
				      "/communication/GC/GCUtil/Container"
				      "/communication/GC/GCUtil/Container/TS"
				      "/communication/GC/GCUtil/String"
				      "/communication/GC/GCUtil/String/UTF8"
				      "/communication/GC/GCUtil/Traits"
				      "/communication/GC/GCUtil/Macro"
				      "/communication/GC/GCUtil/Windows"
				      "/communication/GC/GCUtil/Debug"
				      "/communication/GC/GCUtil/Thread"
				      "/communication/GC/GCUtil/Generic"
				      "/communication/GC/GCUtil/Filesystem"
				      "/communication/GC/Scheduler"
				      "/communication/GC/Scheduler/Queues"
				      "/communication/GC/Scheduler/Queues/UniqueWriteMsgInDeque"
				      "/communication/GC/Scheduler/Queues/SimpleDeque"
				      "/communication/GC/Scheduler/Queues/MultiQueue"
				      "/communication/GC/Scheduler/detail"
				      "/communication/GC/PluginManager"
				      "/communication/GC/CommAdapter"
				      "/communication/GC/CommAdapter/Thc"
				      "/communication/GC/CommAdapter/Thc/Parameters"
				      "/communication/GC/CommAdapter/Thc/Parameters/Private"
				      "/communication/GC/CommAdapter/Thc/Symbolic"
				      "/communication/GC/CommAdapter/Thc/Image"
				      "/communication/GC/CommAdapter/Thc/LibAccess"
				      "/communication/GC/CommAdapter/Direct"
				      "/communication/GC/CommAdapter/detail"
				      "/communication/GC/GCStdLib"
				      "/communication/GC/GCStdLib/Chrono"
				      "/communication/GC/GCStdLib/String"
				      "/communication/GC/GCStdLib/License"
				      "/communication/GC/GCStdLib/Memory"
				      "/communication/GC/GCStdLib/Windows"
				      "/communication/GC/GCStdLib/Windows/Registry"
				      "/communication/GC/GCStdLib/Windows/COM"
				      "/communication/GC/GCStdLib/Windows/_unused"
				      "/communication/GC/GCStdLib/Types"
				      "/communication/GC/GCStdLib/Thread"
				      "/communication/GC/GCStdLib/resource"
				      "/communication/GC/CommManager"
				      "/communication/GC/CommManager/Private"
				      "/communication/GC/ErrorManager"
				      "/communication/GC/ErrorManager/Private"
				      "/communication/GC/CommProtocols"
				      "/communication/GC/CommProtocols/Helper"
				      "/communication/GC/CommProtocols/Ethernet"
				      "/communication/GC/CommProtocols/Ethernet/IP"
				      "/communication/GC/CommProtocols/Ethernet/IP/ICMP"
				      "/communication/GC/CommProtocols/Ethernet/IP/Socket"
				      "/communication/GC/CommProtocols/Ethernet/IP/TCP"
				      )
		      )
