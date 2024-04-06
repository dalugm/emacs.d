;; All the Tempo syntax elements are fully supported. The syntax
;; elements are described in detail in the docstring of
;; `tempo-define-template' in `tempo'.

bash-ts-mode sh-mode

(b "#!/usr/bin/env " (p "sh"))
(case "case " (p "cond") " in" n> p ")" n> (p "expr") n> ";;" n> r> n "esac")
(for "for " p " in " p "; do" n> r> n "done")
(fn "function " p " {" n> r> n "}")
(if "if " (p "pred") "; then" n> r> n "fi")
(select "select " p " in " p "; do" n> r> n "done")
(until "until " p "; do" n> r> n "done")
(while "while " p "; do" n> r> n "done")

c++-mode c++-ts-mode c-mode c-ts-mode dart-mode dart-ts-mode java-mode java-ts-mode

(fn (p "void") " " (p "name") "(" p ") {" n> r> n "}" >)

c++-mode c++-ts-mode c-mode c-ts-mode dart-mode dart-ts-mode java-mode java-ts-mode js-mode js-ts-mode tsx-ts-mode typescript-mode typescript-ts-mode web-mode

(class "class " p " {" n> r> n "}")
(dow "do {" n> p n "} while (" p ")" >)
(for "for (" p ") {" n> r> n "}" >)
(if "if (" p ") {" n> r> n "}" >)
(switch "switch (" p ") {" n> r> n "}" >)
(ter "(" (p "pred") ") ? " (p "then") " : " (p "else") ";")
(while "while(" p ") {" n> r> n "}" >)

c-mode

(p "printf(" r ");")

c-mode :when (re-search-backward "^\\S-*$" (line-beginning-position) 'noerror)

(inc "#include <" (p (concat (file-name-base (or (buffer-file-name) (buffer-name))) ".h")) ">")
(incc "#include \"" (p (concat (file-name-base (or (buffer-file-name) (buffer-name))) ".h")) "\"")
(io "#include <stdio.h>")
(compile "// -*- compile-command: \"gcc -Wall -o " (file-name-sans-extension (buffer-name)) " " (buffer-name) " && ./" (file-name-sans-extension (buffer-name)) "\" -*-")
(main "int main() {" n> p n n> "return 0;" n "}")
(std "#include <stdlib.h>")
(str "#include <string.h>")

clojure-mode clojure-ts-mode

(comment "(comment" n> r> n> ":done)")
(cond "(cond" n> "(" p ") " p n> ":else " r ")")
(condp "(condp " (p "pred") " " (p "expr") n> r> ")")
(def "(def " r ")")
(defn "(defn " p "\n  \"" (p "doc") "\"\n  [" p "]" n> r> ")")
(defn- "(defn- " p "\n  \"" (p "doc") "\"\n  [" p "]" n> r> ")")
(doall "(doall" n> r> ")")
(dorun "(dorun" n> r> ")")
(doseq "(doseq [" p "]" n> r> ")")
(dotimes "(dotimes [" p "]" n> r> ")")
(fn "(fn [" p "] " r ")")
(ifl "(if-let [" p "]" n> r> ")")
(import "(:import (" p "))")
(letfn "(letfn [" p "]" n> r> ")")
(loop "(loop [" p "]" n> r> ")")
(macro "(defmacro " p "\n  \"" (p "doc") "\"\n  [" p "]" n> r> ")")
(map "(map " p " " p ")")
(mapi "(map-indexed " p " " p ")")
(mdoc "^{:doc \"" r "\"}")
(ns "(ns " r ")")
(once "(defonce " r ")")
(pl "(println " r ")")
(prn "(prn " r ")")
(record "(defrecord " p " [" p "]" n> r> ")")
(reduce "(reduce " p " " p ")")
(require "(:require [" p "])")
(try "(try" n> p n> "(catch " (p "Exception") " e" n> r> "))")
(type "(deftype " p " [" p "]" n> r> ")")
(use "(:use [" p "])")
(whenl "(when-let [" p "]" n> r> ")")
(whenn "(when-not " p n> r> ")")

clojure-mode clojure-ts-mode :when (re-search-backward "^\\S-*$" (line-beginning-position) 'noerror)

(main "(defn -main [& args]" n> r> ")")

clojure-mode clojure-ts-mode emacs-lisp-mode fennel-mode lisp-mode racket-mode ;; Specify multiple modes

(and "(and " r ")")
(format "(format " r ")")
(if "(if " (p "pred") n> r> ")")
(not "(not " r ")")
(or "(or " r ")")
(when "(when " (p "pred") n> r> ")")

clojure-mode clojure-ts-mode fennel-mode

(-> "(-> " r ")")
(->> "(->> " r ")")
(case "(case " (p "val") n> r> ")")
(do "(do" n> r> ")")
(doto "(doto " p n> r> ")")
(for "(for [" p "]" n> r> ")")
(let "(let [" p "]" n> r> ")")
(p "(print " r ")")
(while "(while " p n> r> ")")

dart-mode dart-ts-mode

(afn "Future<" (p "void") "> " (p "name") "(" p ") async {" n> r> n "}" >)
(build "@override" n>
       "Widget build(BuildContext context) {" n>
       "return const Placeholder(" n>
       "child: Center(child: Text('" r "'))," n>
       ");" n "}" >)
(cp "const " p "._();")
(dcd "@override" n>
     "void didChangeDependencies () {" n>
     "super.didChangeDependencies();" n> r> n
     "}" >)
(dp "@override" n> "void dispose() {" n> r> n> "super.dispose();" n "}" >)
(fzj "factory " (p "name" name) ".fromJson(Map<String, dynamic> json) => _$" name "FromJson(json);")
(is "@override" n> "void initState() {" n> "super.initState();" n> r> n "}" >)
(p "print(" r ")")
(pv "print('" (r "var" var) ": ${" var "}')")
(ss "setState(() {" n> r> n "});" >)
(try "try {" n> r> n
     "} catch (e, s) { " > n>
     "print('Exception details: $e');" n>
     "print('Stack trace: $s');" n
     "}" >)
(uc "final " (p "cachedFn") " = useCallback(" (p "fn") ", [" r "]);")
(uct "final " (p "value") " = useContext(" r ");")
(ue "useEffect(() {" n> "return " p ";" n> "}, [" r "]);")
(um "final " (p "memoized") " = useMemoized(() => " (p "result") ", [" r "]);")
(ur "const [" (p "state") ", " (p "dispatch") "] = useReducer(" (p "reducer") ", " (p "args") ");")
(urf "final " (p "ref") " = useRef(" r ");")
(us "final " (p "name") " = useState<" (p "dynamic") ">(" r ");")
(uvc "useValueChanged(" (p "value") ", (_, __) {" n> r> ";\n});" >)

dart-mode dart-ts-mode :when (re-search-backward "^\\S-*$" (line-beginning-position) 'noerror)

(arpc "@riverpod" n
      "class " (p "name" name) " extends _$" name " {" n>
      "@override" n>
      "Future<" (p "dynamic") "> build() async => " (p "throw UnimplementedError()") ";" n
      "}")
(arpi "@riverpod" n
      "Future<" (p "dynamic") "> " (p "name" name) "(" (upcase-initials name) "Ref ref) async => " (p "throw UnimplementedError()") ";")
(csw "class " (p "name" name) " extends ConsumerStatefulWidget {" n>
     "const " name "({super.key});" n n>
     "@override" n> "ConsumerState<" name "> createState() => _" name "State();" n
     "}" n n
     "class _" name "State extends ConsumerState<" name "> {" n>
     (i build) n
     "}")
(cw "class " (p "name" name) " extends ConsumerWidget {" n>
    "const " name "({super.key});" n n>
    (i build) n
    "}")
(enum "enum " p " {" n> r> n "}" >)
(fzc "@freezed" n
     "class " (p "name" name) " with _$" name " {" n>
     "const factory " name "({" n>
     r> n>
     "}) = _" name ";" n
     "}")
(gra "@TypedGoRoute<" (p "name") "Route>(path: '" p "')")
(grb "class " (p "name" name) "Route extends GoRouteData {" n>
     "const " name "Route();" n n>
     "@override" n>
     "Widget build(BuildContext context, GoRouterState state) => const " (p "screen") "();" n
     "}")
(hcw "class " (p "name" name) " extends HookConsumerWidget {" n>
     "const " name "({super.key});" n n>
     (i build) n
     "}")
(hw "class " (p "name" name) " extends HookWidget {" n>
    "const " name "({super.key});" n n>
    (i build) n
    "}")
(imp "import '" p "';")
(jsc "@JsonSerializable" n
     "class " (p "name" name) " {" n>
     name "({" n>
     r> n>
     "});" n n>
     "factory " name ".fromJson(Map<string, dynamic> json) => _$" name "FromJson(json);" n n>
     "Map<String, dynamic> toJson() => _$" name "ToJson(this);" n
     "}")
(pf "part '" (file-name-sans-extension (buffer-name)) ".freezed.dart';")
(pg "part '" (file-name-sans-extension (buffer-name)) ".g.dart';")
(po "part of '" p "';")
(pt "part '" p "';")
(rpc "@riverpod" n
     "class " (p "name" name) " extends _$" name " {" n>
     "@override" n>
     (p "dynamic") " build() => " (p "throw UnimplementedError()") ";" n
     "}")
(rpi "@riverpod" n
     (p "dynamic") " " (p "name" name) "(" (upcase-initials name) "Ref ref) => " (p "throw UnimplementedError()") ";")
(sfw "class " (p "name" name) " extends StatefulWidget {" n>
     "const " name "({super.key});" n n>
     "@override" n>
     "State<" name "> createState() => _" name "State();" n
     "}" n n
     "class _" name "State extends State<" name "> {" n>
     (i build) n
     "}")
(shcw "class " (p "name" name) " extends StatefulHookConsumerWidget {" n>
      "const " name "({super.key});" n n>
      "@override" n>
      "ConsumerState<" name "> createState() => _" name "State();" n
      "}" n n
      "class _" name "State extends ConsumerState<" name "> {" n>
      (i build) n
      "}")
(shw "class " (p "name" name) " extends StatefulHookWidget {" n>
     "const " name "({super.key});" n n>
     "@override" n>
     "State<" name "> createState() => _" name "State();" n
     "}" n n
     "class _" name "State extends State<" name "> {" n>
     (i build) n
     "}")
(slw "class " (p "name" name) " extends StatelessWidget {" n>
     "const " name "({super.key});" n n>
     (i build) n
     "}")

dart-mode dart-ts-mode js-mode js-ts-mode tsx-ts-mode typescript-mode typescript-ts-mode

(pcase (p "pattern") " => " (p "expr") ",")

elixir-mode elixir-ts-mode

(after "after " (p "500") " ->" n> r>)
(call "GenServer.call(" (p "__MODULE__") ", " r ")")
(case "case " (p "val") " do" n> r> n "end" >)
(cast "GenServer.cast(" (p "__MODULE__") ", " r ")")
(cond "cond do" n> r> n "end" >)
(d "dbg(" r ")")
(def "def " p " do" n> r> n "end" >)
(defmacro "defmacro " p " do" n> r> n "end" >)
(defmacrop "defmacrop " p " do" n> r> n "end" >)
(defmodule "defmodule " p " do" n> r> n "end" >)
(defp "defp " p " do" n> r> n "end" >)
(do "do" n> r> "end" >)
(doc "@doc \"\"\"" n> r> n "\"\"\"" >)
(fn "fn " p " -> " p " end")
(for "for " p " <- " p " do" n> r> n "end" >)
(hcall "def handle_call(" p ", _from, " (p "state") ") do" n> r> n "end" >)
(hcast "def handle_cast(" p ", " (p "state") ") do" n> r> n "end" >)
(hinfo "def handle_info(" p ", " (p "state") ") do" n> r> n "end" >)
(if "if " p " do" n> r> n "end" >)
(i "IO.inspect("(p "val" label) ", label: \"" label "\")")
(mdoc "@moduledoc \"\"\"" n> r> n "\"\"\"" >)
(rcv "receive do" n> r> n "end" >)
(test "test \"" p "\" do" n> r> n "end" >)
(unless "unless " p " do" n> r> n "end" >)

emacs-lisp-mode lisp-mode

(cond "(cond" n "(" q "))" >)
(defun "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")
(ifl "(if-let (" p ")" n> r> ")")
(lambda "(lambda (" p ")" n> r> ")")
(let "(let (" p ")" n> r> ")")
(lets "(let* (" p ")" n> r> ")")
(local "(defvar-local " r ")")
(param "(defparameter " r ")")
(progn "(progn" n> r> ")")
(setf "(setf " r ")")
(var "(defvar " r ")")
(whenl "(when-let (" p ")" n> r> ")")
(whilel "(while-let (" p ")" n> r> ")")

emacs-lisp-mode

(alias "(defalias '" p " '" p ")")
(andl "(and-let* (" p ")" n> r> ")")
(autoload ";;;###autoload")
(cmd "(defun " p " (" p ")\n  \"" p "\"" n> "(interactive" p ")" n> r> ")")
(const "(defconst " p "\n  \"" p "\")")
(custom "(defcustom " p "\n  \"" p "\"" n> ":type '" p ")")
(dolist "(dolist (" p ")" n> r> ")")
(dotimes "(dotimes (" p ")" n> r> ")")
(face "(defface " p " '((t :inherit " p "))\n  \"" p "\")")
(group "(defgroup " p " nil\n  \"" p "\"" n> ":group '" p n> ":prefix \"" p "-\")")
(loop "(cl-loop for " p " in " p " do" n> r> ")")
(m "(message \"" r "\")")
(macro "(defmacro " p " (" p ")\n  \"" p "\"" n> r> ")")
(pcase "(pcase " (p "scrutinee") n "(" q "))" >)
(pcasel "(pcase-let (" p ")" n> r> ")")
(pcasels "(pcase-let* (" p ")" n> r> ")")
(pt "(point)")
(rec "(letrec (" p ")" n> r> ")")
(advice "(defun " (p "adv" name) " (&rest app)" n> p n> "(apply app))" n>
        "(advice-add #'" (p "fun") " " (p ":around") " #'" (s name) ")")
(header ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
        " -- " p " -*- lexical-binding: t -*-" n
        ";;; Commentary:" n ";;; Code:" n n)
(provide "(provide '" (file-name-base (or (buffer-file-name) (buffer-name))) ")" n
         ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
         " ends here" n)
(package (i header) r n n (i provide))

eshell-mode

(for "for " (p "i") " in " p " { " q " }")
(while "while { " p " } { " q " }")
(until "until { " p " } { " q " }")
(if "if { " p " } { " q " }")
(ife "if { " p " } { " p " } { " q " }")
(unl "unless { " p " } { " q " }")
(unle "unless { " p " } { " p " } { " q " }")

fennel-mode

(collect "(collect [" p "]" n> r> ")")
(each "(each [" p "]" n> r> ")")
(fn "(fn " (p "name") " [" p "]" n> "\"" (p "doc") "\"" n> r> ")")
(icollect "(icollect [" p "]" n> r> ")")
(ipairs "(ipairs " r ")")
(lambda "(lambda " (p "name") " [" p "]" n> r> ")")
(local "(local " r ")")
(match "(match " p n> r> ")")
(pairs "(pairs " r ")")
(require "(require :" r ")")
(set "(set " r ")")
(tin "(table.insert " r ")")
(trm "(table.remove " r ")")
(tset "(tset " r ")")
(var "(var " r ")")

fundamental-mode ;; Available everywhere

(today (format-time-string "%Y-%m-%d"))

heex-mode heex-ts-mode html-mode html-ts-mode nxml-mode web-mode

(a "<a href=\"" (p "#") "\">" r "</a>")
(body "<body>" n> r> n "</body>" >)
(br "<br />" >)
(doc "<!DOCTYPE html>")
(doc11 "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")
(doc4 "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">")
(form "<form method=\"" p "\" action=\"" p "\">" n> r> n "</form>" >)
(head "<head>" n> r> n "</head>" >)
(html "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"" (p "en" en) "\" lang=\"" en "\">" n> r> n "</html>" >)
(img "<img src=\"" p "\" alt=\"" p "\" />")
(input "<input type=\"" p "\" name=\"" p "\" value=\"" p "\" />")
(link "<link ref=\"" (p "stylesheet") "\" href=\"" (p "url") "\" type=\"" (p "text/css") "\" media=\"" (p "screen") "\" />")
(meta "<meta name=\"" p "\" content=\"" p "\" />")
(quote "<blockquote>" n> r> n "</blockquote>" >)
(style "<style tyle=\"text/css\" media=\"" (p "screen") "\">" n> r> n "</style>")
(tag "<" (p "tag" tag) ">" r "</" tag ">")
(tags "<" p " />")
(tpl "<!DOCTYPE html>" n
     "<html lang=\"en\">" n
     "<head>" > n
     "<meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\" />" > n
     "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\" />" > n
     "<title>" (r "HTML5 template") "</title>" > n
     "</head>" > n
     "<body>" > n
     "<div id=\"app\"></div>" > n
     "</body>" > n
     "</html>")

java-mode java-ts-mode

(p "System.out.print(" p ");")
(pf "System.out.printf(" p ");")
(pl "System.out.println(" p ");")

js-mode js-ts-mode tsx-ts-mode typescript-mode typescript-ts-mode web-mode

(cas "console.assert(" (p "assertion") ");")
(ccl "console.clear();")
(cer "console.error(" r ");")
(cin "console.info(" r ");")
(clg "console.log(" r ");")
(clf "console.log('" (p "var" var) ": ', " var ");")
(cli "console.log(`" r "`);")
(cwn "console.warn(" r ");")
(fn "function " p "(" p ") {" n> r> n "}" >)
(afn "async " (i fn))
(ip "import " r ";")
(try "try {" n> p n "} catch (err) {" > n> p n "} finally {" > n> r> n "}" >)
(uc "const " (p "cachedFn") " = useCallback(" (p "fn") ", [" r "]);")
(uct "const " p " = useContext(" r ");")
(ue "useEffect(() => {" n> "return " p ";" n "}, [" r "]);" >)
(um "const " (p "memoized") " = useMemo(() => " (p "rst") ", [" r "]);")
(ur "const [" (p "state") ", " (p "dispatch") "] = useReducer(" (p "reducer") ", " (p "args") ");")
(urf "const " (p "ref") " = useRef(" r ");")
(us "const [" (p "var" var) ", set" (upcase-initials var) "] = useState(" r ");")

latex-mode

(abstract "\\begin{abstract}\n" r> n> "\\end{abstract}")
(align "\\begin{align}\n" r> n> "\\end{align}")
(alignn "\\begin{align*}\n" r> n> "\\end{align*}")
(gather "\\begin{gather}\n" r> n> "\\end{gather}")
(gatherr "\\begin{gather*}\n" r> n> "\\end{gather*}")
(appendix "\\begin{appendix}\n" r> n> "\\end{appendix}")
(begin "\\begin{" (s env) "}" r> n> "\\end{" (s env) "}")
(center "\\begin{center}\n" r> n> "\\end{center}")
(displaymath "\\begin{displaymath}\n" r> n> "\\end{displaymath}")
(document "\\begin{document}\n" r> n> "\\end{document}")
(enumerate "\\begin{enumerate}\n\\item " r> n> "\\end{enumerate}")
(equation "\\begin{equation}" r> n> "\\end{equation}")
(flushleft "\\begin{flushleft}" r> n> "\\end{flushleft}")
(flushright "\\begin{flushright}" r> n> "\\end{flushright}")
(frac "\\frac{" p "}{" q "}")
(fussypar "\\begin{fussypar}" r> n> "\\end{fussypar}")
(itemize "\\begin{itemize}\n\\item " r> n> "\\end{itemize}")
(letter "\\begin{letter}\n" r> n> "\\end{letter}")
(math "\\begin{math}\n" r> n> "\\end{math}")
(minipage "\\begin{minipage}[t]{0.5\linewidth}\n" r> n> "\\end{minipage}")
(quotation "\\begin{quotation}\n" r> n> "\\end{quotation}")
(quote "\\begin{quote}\n" r> n> "\\end{quote}")
(sloppypar "\\begin{sloppypar}\n" r> n> "\\end{sloppypar}")
(theindex "\\begin{theindex}\n" r> n> "\\end{theindex}")
(trivlist "\\begin{trivlist}\n" r> n> "\\end{trivlist}")
(verbatim "\\begin{verbatim}\n" r> n> "\\end{verbatim}")
(verbatimm "\\begin{verbatim*}\n" r> n> "\\end{verbatim*}")

lisp-mode

(export "(:export " r ")")
(use "(:use " r ")")

markdown-mode

(toc (let ((start "# Table of Contents")
           (end "<!-- :TOC: -->"))
       (concat start
               (make-string
                (max (- fill-column
                        (length start)
                        (length end))
                     1)
                ? )
               end)))

org-mode

(ascii "#+begin_export ascii" n> r> n "#+end_export")
(author "#+author: " r)
(begin "#+begin_" (s name) n> r> n "#+end_" name)
(caption "#+caption: ")
(center "#+begin_center" n> r> n "#+end_center")
(comment "#+begin_comment" n> r> n "#+end_comment")
(date "#+date: " (format-time-string "%FT%T%z"))
(desc "#+description: " r)
(drawer ":" p ":" n r ":end:")
(elisp "#+begin_src emacs-lisp" n r n "#+end_src" :post (org-edit-src-code))
(email "#+email: " r)
(example "#+begin_example" n> r> n "#+end_example")
(gnuplot "#+begin_src gnuplot :var data=" (p "table") " :file " (p "plot.png") n r n "#+end_src" :post (org-edit-src-code))
(html "#+begin_export html" n> r> n "#+end_export")
(inlsrc "src_" p "{" q "}")
(lang "#+language: " (r "en"))
(latex "#+begin_export latex" n> r> n "#+end_export")
(ltxb "\\begin{" (p "env" env) "}" n> r> n "\\end{" env "}")
(ltxbx "#+latex: \\resizebox{\\textwidth}{!}{" n> r> n "#+latex: }")
(ltxc "#+latex_header: \\usepackage{ctex}")
(ltxci "#+latex: \\begin{adjustwidth}{1.2em}{0em}" n> r> n "#+latex: \\end{adjustwidth}")
(ltxh "#+latex_header: " r)
(ltxtp "#+latex_header: \\makeatletter \\def\\@maketitle{\\null \\begin{center} {\\vskip 5em \\Huge \\@title} \\vskip 30em {\\LARGE \\@author} \\vskip 3em {\\LARGE \\@date} \\end{center} \\newpage} \\makeatother")
(ltxts "#+latex_class_options: [a4paper,twoside,twocolumn]")
(ltxu "#+latex_header: \\usepackage{" r "}")
(marginnote "#+begin_marginnote" n> r> n "#+end_marginnote")
(quote "#+begin_quote" n> r> n "#+end_quote")
(sidenote "#+begin_sidenote" n> r> n "#+end_sidenote")
(src "#+begin_src " p n r n "#+end_src")
(title "#+title: " r)
(toc (let ((start "* Table of Contents")
           (end ":TOC:noexport:"))
       (concat start
               (make-string
                (max (- (abs org-tags-column)
                        (length start)
                        (length end))
                     1)
                ? )
               end)))
(verse "#+begin_verse" n> r> n "#+end_verse")

prog-mode

(fixme (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME ")
(todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO ")
(bug (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "BUG ")
(hack (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "HACK ")

racket-mode

(case "(case " (p "val") n> "[" p "]" n> "[else " r "])")
(cond "(cond" n> "[" (p "pred") " " p "]" n> "[else " r "]" ")")
(d "(display " r ")")
(dl "(displayln " r ")")

rst-mode

(title (make-string (length title) ?=) n (p "Title: " title) n (make-string (length title) ?=) n)

rust-mode rust-ts-mode

(afn "async " (i fn))
(afnr "async " (i fnr))
(afns "async " (i fns))
(afnw "async " (i fnw))
(allow "#[allow(" (p "lint") ")]")
(allow! "#![allow(" (p "lint") ")]")
(anfn "|" (p "arg") "| {" n> r> n "}" >)
(asrt "assert!(" (p "pred") ")")
(aseq "assert_eq!(" (p "expected") ", " (p "actual") ");")
(case (p "pattern") " => " (p "expr") ",")
(cfg "#[cfg(" (p "opt") ")]")
(cfgv "#[cfg(" (p "opt") " = \"" (p "val") "\")]")
(d "dbg!(" r ")")
(deny "#[deny(" (p "lint") ")]")
(deny! "#![deny(" (p "lint") ")]")
(derive "#[derive(" (p "Trait") ")]")
(display "impl Display for " p " {" n>
         "fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {" n>
         "write!(f, \"" r "\")" n> "}" n
         "}")
(enum "enum " p " {" n> r> n> "}")
(ep "eprint!(" r ");")
(epl "eprintln!(" r ");")
(fn "fn " (p "name") "(" p ") {" n> r> n "}" >)
(fnr "fn " (p "name") "(" p ") -> " p " {" n> r> n "}" >)
(fns "fn " (p "name") "(&self" p ") {" n> r> n "}" >)
(fnw "fn " (p "name") "<" p ">(" p ") where " p " {" n> r> n "}" >)
(for "for " p " in " p " {" n> r> n "}" >)
(if "if " p " {" n> r> n "}" >)
(ifl "if let " (p "pattern") " = " (p "expr") " {" n> r> n "}" >)
(impl "impl " p " {" n> r> n "}")
(implf "impl " p " for " p " {" n> r> n "}")
(loop "loop {" n> r> n "}" >)
(macro "macro_rules! " p " {" n> r> n> "}")
(main "fn main() {" n> r> n "}")
(match "match " (p "expr") " {" n> r> n "}" >)
(mod "mod " (p "module") " {" n> r> n "}" >)
(pfn "pub " (i fn))
(pfnr "pub " (i fnr))
(pfns "pub " (i fns))
(pfnw "pub " (i fnw))
(p "print!(" r ");")
(pl "println!(" r ");")
(spawn "spawn(proc() {" n> r> n "});" >)
(struct "struct " p " {" n> r> n> "}")
(test "#[test]" n (i fn))
(trait "trait " p " {" n> r> n> "}")
(union "union " p " {" n> r> n "}")
(warn "#[warn(" (p "lint") ")]")
(warn! "#![warn(" (p "lint") ")]")
(while "while " p " {" n> r> n "}" >)
(whilel "while let " (p "pattern") " = " (p "expr") " {" n> r> n "}" >)

srt-mode subed-srt-mode text-mode

(srt p ":" p ":" p "," p " --> " p ":" p ":" p "," p n
     r)

texinfo-mode

(defmac "@defmac " p n> r> "@end defmac")
(defun "@defun " p n> r> "@end defun")
(defvar "@defvar " p n> r> "@end defvar")
(example "@example " p n> r> "@end example")
(lisp "@lisp " p n> r> "@end lisp")
(bullet "@itemize @bullet{}" n> r> "@end itemize")
(code "@code{" p "}")
(var "@var{" p "}")

text-mode

(box "┌─" (make-string (length str) ?─) "─┐" n
     "│ " (s str)                       " │" n
     "└─" (make-string (length str) ?─) "─┘" n)
(abox "+-" (make-string (length str) ?-) "-+" n
      "| " (s str)                       " |" n
      "+-" (make-string (length str) ?-) "-+" n)
(fm (p "content") n n
    "        [32m《" p "》[m [33m—— " r "[m" n
    "%")
(fp "[32m《" p "》[m" n
    "[33m" p "[m" n
    r n
    "%")
(cut "--8<---------------cut here---------------start------------->8---" n r n
     "--8<---------------cut here---------------end--------------->8---" n)
(rot13 (p "plain text" text) n "----" n (rot13 text))
(calc (p "taylor(sin(x),x=0,3)" formula) n "----" n (format "%s" (calc-eval formula)))

typst-mode typst-ts-mode

(break "#pagebreak()")
(code "```" (p "lang") n> r n> "```")
(codei "```" (p "lang") " " r "```")
(image "#image(\"" r "\")")
(link "#link(\"" p "\")[" r "]")
(set "#set " r)
(show "#show " r)
(toc "#show outline.entry.where(" n>
     "level: 1" n>
     "): it => {" n>
     "v(12pt, weak: true)" n>
     "strong(it)" n>
     "}" n>
     "#outline(indent: auto)")

;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
