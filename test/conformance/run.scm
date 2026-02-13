(add-load-path "./test/conformance")
(load "common.scm")
(load "milestones.scm")

(exit (run-all all-milestones))
