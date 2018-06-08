(add-to-list 'load-path "~/git/github/gnull/cfparser")
;; https://derdaniel.me/emcas-how-to-check-if-a-package-is-installed
(when (package-installed-p 'cf-mode)
  (require 'cf-mode)
  (add-hook 'find-file-hook 'cf-mode)  ; enable cf-mode for all open files
  (setq cf-test-command
	(concat
	 "set -eo pipefail; "
	 ;; "g++ -g -Wall -Wextra -pedantic -std=c++11 -O2 -Wshadow -Wformat=2 -Wfloat-equal -Wconversion -Wlogical-op -Wshift-overflow=2 -Wduplicated-cond -Wcast-qual -Wcast-align -D_GLIBCXX_DEBUG -D_GLIBCXX_DEBUG_PEDANTIC -D_FORTIFY_SOURCE=2 -fsanitize=address -fsanitize=undefined -fno-sanitize-recover -fstack-protector sol.cpp; "
	 ;; "g++ -g -Wall -Wextra -O2 -Wno-misleading-indentation -D_GLIBCXX_DEBUG -D_FORTIFY_SOURCE=2 sol.cpp; "
	 "g++ -g -Wall -Wextra -pedantic -std=c++17 -O2 -Wformat=2 -Wfloat-equal -Wlogical-op -Wshift-overflow=2 -Wduplicated-cond -Wcast-qual -Wcast-align -D_GLIBCXX_DEBUG -D_GLIBCXX_DEBUG_PEDANTIC -D_FORTIFY_SOURCE=2 -fsanitize=undefined -fno-sanitize-recover -fstack-protector -Wno-misleading-indentation sol.cpp; "
	 "for i in `ls *.in | sed 's/.in//'`; do "
	 "  echo \"\nTest #$i:\"; "
	 "  echo \"Running...\"; "
	 "  ./a.out < $i.in | tee out; "
	 "  if [[ -f $i.out ]]; then "
	 "    echo \"Checking...\"; "
	 "    diff -b out $i.out && rm -f out; "
	 "  else "
	 "    echo \"No output file...\"; "
	 "  fi; "
	 "done;")))
