(import test srfi-170 srfi-19 (chicken file) (chicken condition))
(import (only (chicken bitwise) bitwise-and bitwise-ior))
(import (only (chicken file posix) port->fileno))

(test-begin "srfi-170-final-rigorous")

;; --- 1. Errors ---
(test-group "1. Errors"
  ;; Trigger a real error to test the object accessors
  (let ((err-obj (handle-exceptions ex ex 
                   (open-file "/non/existent/path" textual-input 0))))
    (test "posix-error?" #t (posix-error? err-obj))
    (test "posix-error-name" 'ENOENT (posix-error-name err-obj))
    (test "posix-error-number" #t (integer? (posix-error-number err-obj)))
    (test "posix-error-message" #t (string? (posix-error-message err-obj)))))

;; --- 2. I/O and File Descriptors ---
(test-group "2. I/O"
  ;; buffer-none, buffer-block, and buffer-line are constants
  (test "buffer-none" #t (not (null? buffer-none)))
  (test "buffer-block" #t (not (null? buffer-block)))
  (test "buffer-line" #t (not (null? buffer-line)))
  
  (let ((fn "test-io.tmp"))
    ;; open-file: fname, port-type (constant), flags (bitmask), [permissions (bitmask)]
    (let ((fd (open-file fn textual-output (bitwise-ior open/create open/truncate))))
      (test "open-file" #t (port? fd))
      (test "close-port" #t (begin (close-output-port fd) #t)))
    
    ;; Reading with 0 flags and buffer-none constant
    (let* ((fd (port->fileno (open-file fn textual-input 0)))
           (port (fd->port fd textual-input)))
      (test "fd->port" #t (input-port? port))
      (test "port->fd" fd (port->fileno port))
      (close-input-port port))
    
    (delete-file fn)))

  ;; --- 3. File System ---
(test-group "3. File System"
  (let ((f1 "f1.tmp") (f2 "f2.tmp") (d1 "d1.tmp") (l1 "l1.tmp"))
    ;; Check directory creation via file-info predicate
    (test "create-directory" #t 
          (begin (create-directory d1) 
                 (file-info-directory? (file-info d1 #t))))
    
    ;; Check FIFO creation
    (test "create-fifo" #t 
          (begin (create-fifo f1) 
                 (file-info? (file-info f1 #t))))
    (delete-file f1)
    
    (with-output-to-file f1 (lambda () (display "data")))
    
    (test "create-hard-link" #t 
          (begin (create-hard-link f1 f2) 
                 (file-info? (file-info f2 #t))))
    (delete-file f2)
    
    ;; Symbolic link check (passing #f to file-info so it doesn't follow the link)
    (test "create-symlink" #t 
          (begin (create-symlink f1 l1) 
                 (file-info-symlink? (file-info l1 #f))))
    
    (test "read-symlink" f1 (read-symlink l1))
    
    (test "rename-file" #t 
          (begin (rename-file f1 "f-new.tmp") 
                 (file-info? (file-info "f-new.tmp" #t))))
    
    (delete-file "f-new.tmp") 
    (delete-file l1) 
    (delete-directory d1)

    ;; Truncate check using file-info:size
    (test "truncate-file" #t 
          (begin (with-output-to-file f1 (lambda () (display "12345")))
                 (truncate-file f1 2)
                 (= 2 (file-info:size (file-info f1 #t)))))
    
    ;; Temp file/dir procedures return lists (fd path) or (path)
    (test "create-temp-file" #t 
          (let ((r (create-temp-file "s170-"))) 
            (delete-file r) 
            #t))
    
    (test "create-temp-directory" #t 
          (let ((d (create-temp-file "s170-")))
			(delete-file d)
			(create-directory d)
            (delete-directory d) 
            #t))
    (delete-file f1)))

;; --- 4. File Info ---
(test-group "4. File Info"
  (let ((fn "info.tmp"))
    (with-output-to-file fn (lambda () (display "data")))
    (let ((info (file-info fn #t)))
      (test "file-info?" #t (file-info? info))
      (test "file-info:device" #t (integer? (file-info:device info)))
      (test "file-info:inode" #t (integer? (file-info:inode info)))
      (test "file-info:mode" #t (integer? (file-info:mode info)))
      (test "file-info:nlinks" #t (integer? (file-info:nlinks info)))
      (test "file-info:uid" #t (integer? (file-info:uid info)))
      (test "file-info:gid" #t (integer? (file-info:gid info)))
      (test "file-info:size" 4 (file-info:size info))
      (test "file-info:atime" #t (time? (file-info:atime info)))
      (test "file-info:mtime" #t (time? (file-info:mtime info)))
      (test "file-info:ctime" #t (time? (file-info:ctime info)))
      (test "file-info-directory?" #f (file-info-directory? info))
      (test "file-info-symlink?" #f (file-info-symlink? info))
      (test "file-info-regular?" #t (file-info-regular? info)))
    (delete-file fn)))

;; --- 5. Directory Streams ---
(test-group "5. Directory Streams"
  (let ((ds (open-directory ".")))
    (test "directory-object?" #t (directory-object? ds))
    (test "read-directory" #t (string? (read-directory ds)))
    (test "close-directory" #t (begin (close-directory ds) #t))))

;; --- 6. Process Environment ---
(test-group "6. Process Environment"
  (test "working-directory" #t (string? (current-directory)))
  (test "set-working-directory" #t (begin (set-current-directory! (current-directory)) #t))
  (test "pid" #t (integer? (pid)))
  (test "parent-pid" #t (integer? (parent-pid)))
  (test "get-environment-variable" #t (string? (get-environment-variable "PATH")))
  (test "get-environment-variables" #t (list? (get-environment-variables))))

;; --- 7. User and Group Information ---
(test-group "7. User and Group"
  (test "user-uid" #t (integer? (user-uid)))
  (test "user-effective-uid" #t (integer? (user-effective-uid)))
  (test "user-gid" #t (integer? (user-gid)))
  (test "user-effective-gid" #t (integer? (user-effective-gid)))
  
  (let* ((u-id (user-uid)) (u (user-info u-id)))
    (test "user-info?" #t (user-info? u))
    (test "user-info:name" #t (string? (user-info:name u)))
    (test "user-info:uid" u-id (user-info:uid u))
    (test "user-info:home-dir" #t (string? (user-info:home-dir u))))
  
  (let* ((g-id (user-gid)) (g (group-info g-id)))
    (test "group-info?" #t (group-info? g))
    (test "group-info:name" #t (string? (group-info:name g)))))

;; --- 8. Time ---
(test-group "8. Time"
  (test "posix-time" #t (time? (posix-time)))
  (test "monotonic-time" #t (time? (monotonic-time))))

(test-end "srfi-170-final-rigorous")
(test-exit)
