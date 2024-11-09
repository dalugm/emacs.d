;; All the Tempo syntax elements are fully supported. The syntax
;; elements are described in detail in the docstring of
;; `tempo-define-template' in `tempo'.

bash-ts-mode sh-mode

(b "#!/usr/bin/env " (p "sh"))
(case "case \"$" (p "VAR") "\" in" n>
      (p "1") ") echo 1" n> ";;" n>
      (p "2|3") ") echo 2 or 3" n> ";;" n>
      "*) echo default" n> ";;" n> "esac" >)
(for "for ((" (p "i" i) " = 0; " i " < " (p "10") "; " i "++)); do" n> "echo \"$" i "\"" n "done")
(fori "for " p " in " p "; do" n> r> n "done")
(fn (p "name") "() {" n> r> n "}")
(if "if " (p "pred") "; then" n> r> n "fi")
(elif "elif " (p "pred") "; then" n> r>)
(else "else" n> r>)
(select "select " p " in " p "; do" n> r> n "done")
(until "until " p "; do" n> r> n "done")
(while "while " p "; do" n> r> n "done")

c++-mode c++-ts-mode c-mode c-ts-mode dart-mode dart-ts-mode java-mode java-ts-mode

(fn (p "void") " " (p "name") "(" p ") {" n> r> n "}" >)

c++-mode c++-ts-mode c-mode c-ts-mode dart-mode dart-ts-mode java-mode java-ts-mode js-mode js-ts-mode tsx-ts-mode typescript-mode typescript-ts-mode web-mode

(doc "/**" n " * " r n " */")
(dowhile "do {" n> p n "} while (" p ")" >)
(for "for (" p ") {" n> r> n "}" >)
(if "if (" p ") {" n> r> n "}" >)
(switch "switch (" (p "key") ") {" n> "case " (p "value") ":" > n> r> n "break;" > n> "default:" > n> "break;" > n "}" >)
(te "(" (p "pred") ") ? " (p "then") " : " (p "else") ";")
(while "while(" p ") {" n> r> n "}" >)

c++-mode c++-ts-mode

(cin "std::cin >> " r ";")
(cout "std::cout << " r ";")
(class "class " (p "MyClass" class) " {" n
       "public:" n>
       class "();" > n>
       class "(" class " &&) = default;" > n>
       class "(const " class "&) = default;" > n>
       class " &operator=(" class " &&) = default;" > n>
       class " &operator=(const " class " &) = default;" > n>
       "~" class "();" > n n
       "private:" n> r> n
       "};" n n
       class "::" class "() {}" n n
       class "::~" class "() {}")
(classi "class " (p "MyClass" class) " {" n
        "public:" n>
        class "() = default;" > n>
        class "(" class " &&) = default;" > n>
        class "(const " class "&) = default;" > n>
        class " &operator=(" class " &&) = default;" > n>
        class " &operator=(const " class " &) = default;" > n>
        "~" class "() = default;" > n n
        "private:" n> r> n
        "};")
(classe "class " (p "MyClass" class) " {" n
        "public:" n> p n
        "private:" n> r> n
        "};")
(fore "for (auto " (p "elem") " : " (p "collection") ") {" n> r> n "}")
(io "#include <iostream>")
(ns "namespace " (p "Namespace") " {" n> r> n "}")

c++-mode c++-ts-mode c-mode c-ts-mode

(def "#define " (p "MACRO"))
(deff "#define " (p "MACRO") "(" p ") (" r ")")
(enum "enum " (p "Enum") " {" n> r> n "};")
(err "#error \"" r "\"")
(fnd (p "void") " " (p "name") "(" p ");")
(main "int main(int argc, char *argv[]) {" n> p n n> "return 0;" n "}")
(ifd "#if " (p "cond" cond) n r n "#endif /* if " cond " */")
(ifdef "#ifdef " (p "cond" cond) n r n "#endif /* ifdef " cond " */")
(ifndef "#ifndef " (p "cond" cond) n r n "#endif /* ifndef " cond " */")
(init (i io) n n (i main))
(inc "#include <" r ">")
(incl "#include \"" r "\"")
(p "printf(" r ");")
(pv "printf(\"" (p "var" var) " = %" (p "d") "\", " var ");")
(warn "#warning \"" r "\"")

c-mode c-ts-mode :when (re-search-backward "^\\S-*$" (line-beginning-position) 'noerror)

(ass "assert(" r ");")
(assh "#include <assert.h>")
(compile "// -*- compile-command: \"gcc -Wall -o " (file-name-sans-extension (buffer-name)) " " (buffer-name) " && ./" (file-name-sans-extension (buffer-name)) "\" -*-")
(io "#include <stdio.h>")
(lib "#include <stdlib.h>")
(once "#ifndef " (p "FILE" file) "_H" n "#define " file "_H" n r n "#endif /* end of include guard: " file "_H */")
(sass "static_assert(" (p "expr") ", " (p "message") ");")
(st "struct " (p "name") " {" n> r> n "};")
(str "#include <string.h>")
(td "typedef " (p "void") " " (p "alias") ";")
(tds "typedef struct " p " " (p "alias") ";")
(tdu "typedef union " p " " (p "alias") ";")

clojure-mode clojure-ts-mode

(comment "(comment" n> p n> ":" (p "done") ")")
(cond "(cond" n> "(" p ") " p n> ":else " r ")")
(condp "(condp " (p "pred") " " (p "expr") n> r> ")")
(def "(def " r ")")
(defn "(defn " (p "fn") n> "\"" (p "doc") "\"" > n> "[" p "]" > n> r> ")" >)
(defn- "(defn- " (p "fn") n> "\"" (p "doc") "\"" > n> "[" p "]" > n> r> ")" >)
(doall "(doall" n> r> ")")
(dorun "(dorun" n> r> ")")
(doseq "(doseq [" p "]" n> r> ")")
(dotimes "(dotimes [" p "]" n> r> ")")
(fn "(fn [" p "] " r ")")
(ifl "(if-let [" p "]" n> r> ")")
(import "(:import [" p "])")
(letfn "(letfn [" p "]" n> r> ")")
(loop "(loop [" p "]" n> r> ")")
(macro "(defmacro " (p "macro") n> "\"" (p "doc") "\"" > n> "[" p "]" n> r> ")")
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
(run "(run! " r ")")
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
(uvc "useValueChanged(" (p "value") ", (_, __) {" n> r> n> "});" >)

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
(defun "(defun " (p "name") " (" p ")" n> "\"" (p "doc") "\"" n> r> ")")
(lambda "(lambda (" p ")" n> r> ")")
(let "(let (" p ")" n> r> ")")
(lets "(let* (" p ")" n> r> ")")
(local "(defvar-local " r ")")
(param "(defparameter " r ")")
(progn "(progn" n> r> ")")
(setf "(setf " r ")")
(unless "(unless " (p "pred") n> r> ")")
(var "(defvar " r ")")
(whilel "(while-let (" p ")" n> r> ")")

emacs-lisp-mode

(alias "(defalias '" p " '" p ")")
(andl "(and-let* (" p ")" n> r> ")")
(autoload ";;;###autoload")
(cmd "(defun " (p "fn") " (" p ")" n> "\"" (p "doc") "\"" n> "(interactive" p ")" n> r> ")")
(const "(defconst " (p "const") n> "\"" (p "doc") "\")")
(custom "(defcustom " (p "custom") n> "\"" (p "doc") "\"" n> ":type '" r ")")
(dolist "(dolist (" p ")" n> r> ")")
(dotimes "(dotimes (" p ")" n> r> ")")
(face "(defface " p " '((t :inherit " p "))" n> "\"" (p "doc") "\")")
(group "(defgroup " (p "group") " nil" n> "\"" (p "doc") "\"" n> ":group '" p n> ":prefix \"" p "-\")")
(ifl "(if-let* (" p ")" n> r> ")")
(letrec "(letrec (" p ")" n> r> ")")
(loop "(cl-loop for " p " in " p " do" n> r> ")")
(m "(message \"" r "\")")
(macro "(defmacro " (p "macro") " (" p ")" n> "\"" (p "doc") "\"" n> r> ")")
(pcase "(pcase " (p "scrutinee") n "(" q "))" >)
(pcasel "(pcase-let (" p ")" n> r> ")")
(pcasels "(pcase-let* (" p ")" n> r> ")")
(pt "(point)")
(whenl "(when-let* (" p ")" n> r> ")")
(advice "(defun " (p "adv" name) " (&rest app)" n> p n> "(apply app))" n>
        "(advice-add #'" (p "fun") " " (p ":around") " #'" (s name) ")")
(header ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
        " -- " p " -*- lexical-binding: t -*-" n
        ";;; Commentary:" n ";;; Code:" n n)
(provide "(provide '" (file-name-base (or (buffer-file-name) (buffer-name))) ")" n
         ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name))) " ends here")
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

go-mode go-ts-mode

(arr "[" p "]" (p "type") "{" q "}")
(const "const (" n> (p "name") " = " (p "value") n ")")
(fn "func " (p "name") "(" p ") {" n> q n "}" >)
(fngo "go func(" p ") {" n> q n "}()" >)
(fnm "func (" (p "recv") " " (p "type") ") " (p "name") "(" p ") {" n> q n "}" >)
(fnr "func " (p "name") "(" p ") " (p "type") " {" n> q n "}" >)
(for "for " p " {" n> q n "}" >)
(forr "for " (p "k") ", " (p "v") " := range " (p "target") " {" n> q n "}" >)
(fp "fmt.Fprint(" q ")")
(fpf "fmt.Fprintf(" q ")")
(fpl "fmt.Fprintln(" q ")")
(if "if " p " {" n> q n "}" >)
(iferr "if err != " (p "nil") " {" n> q n "}" >)
(imp "import (" n> "\"" (p "pkg")  "\"" n ")")
(inf "interface {" n> q n "}")
(init "func init() {" n> q n "}")
(lp "log.Print(" q ")")
(lpf "log.Printf(\"" q "\")")
(lpl "log.Println(" q ")")
(main "func main() {" n> q n "}")
(map "map[" p "]" q)
(p "fmt.Print(" q ")")
(pf "fmt.Printf(\"" q "\")")
(pkg "package " (p (or (file-name-nondirectory (directory-file-name (file-name-directory (file-name-directory (buffer-file-name))))) "main")))
(pkgt "package " (p (or (file-name-nondirectory (directory-file-name (file-name-directory (file-name-directory (buffer-file-name))))) "main")) "_test")
(pl "fmt.Println(" q ")")
(select "select {" n> "case " p ":" n> q n "}" >)
(sp "fmt.Sprint(" q ")")
(spf "fmt.Sprintf(\"" q "\")")
(spl "fmt.Sprintln(" q ")")
(st "struct {" n> q n "}" >)
(switch "switch " p " {" n> "case " p ":" n> p n> "default:" n> q n "}" >)
(test "func Test" p "(t *testing.T) { " n> q n "}")
(tst "type " (p "name") " " (i st))
(type "type (" n> (p "NewType") " " (p "SourceType") n ")")
(var "var (" n> (p "name") " " (p "type") " = " (p "value") n ")")

haskell-mode haskell-ts-mode

(case "case " (p "var") " of" n>
      (p "cond") " -> " (p "val") n>
      "otherwise -> " (p "other"))
(class "class " (p "Name") " where" n> q)
(data "data " (p "Type") " = " q)
(doc "{-" n q n "-}")
(fn (p "function" fn) " :: " (p "Type") n
    fn " " (p "args") " " q)
(main "main :: IO ()" n "main = do " q)
(module "module " (p "Name") " where" n> q)
(p "print " q)
(prag "{-# " (p "PRAGMA") " #-}")

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
(html5 "<!DOCTYPE html>" n
       "<html lang=\"" (p "en") "\">" n
       "<head>" > n
       "<meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\" />" > n
       "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\" />" > n
       "<title>" (p "HTML5 template") "</title>" > n
       "<link href=\"" (p "css/style.css") "\" rel=\"stylesheet\" />" > n
       "</head>" > n
       "<body>" > n
       "<div id=\"app\"></div>" > n
       "</body>" > n
       "</html>")
(img "<img src=\"" p "\" alt=\"" p "\" />")
(input "<input type=\"" p "\" name=\"" p "\" value=\"" p "\" />")
(link "<link ref=\"" (p "stylesheet") "\" href=\"" (p "url") "\" type=\"" (p "text/css") "\" media=\"" (p "screen") "\" />")
(meta "<meta name=\"" p "\" content=\"" p "\" />")
(quote "<blockquote>" n> r> n "</blockquote>" >)
(style "<style tyle=\"text/css\" media=\"" (p "screen") "\">" n> r> n "</style>")
(tag "<" (p "tag" tag) ">" r "</" tag ">")
(tags "<" p " />")

hy-mode hy-ts-mode

(cond "(cond" n> (p "pred") n> p n> "True" n> q ")")
(cut "(cut " (p "var") " " (p "range") ")")
(defclass "(defclass " (p "name") " [" p "]" n> "(defn __init__ " "[self" p "]" n> q "))")
(defn "(defn " (p "name") " [" p "]" n> "\"" (p "doc") "\"" n> r> ")")
(do "(do" n> r> ")")
(fn "(fn [" p "] " q ")")
(for "(for [" p "]" n> r> ")")
(lambda "(lambda " (p "name") " [" p "]" n> r> ")")
(lfor "(lfor " (p "var") " " (p "list") " " q ")")
(get "(get " (p "var") " " (p "0") ")")
(if "(if " (p "pred") n> (p "then") n> (p "else") ")")
(import "(import " q ")")
(print "(print " q ")")
(require "(require " q ")")
(set "(set " r ")")
(setv "(setv " r ")")
(while "(while " (p "pred") n> r> ")")

java-mode java-ts-mode js-mode js-ts-mode tsx-ts-mode typescript-mode typescript-ts-mode web-mode

(class "class " p " {" n> r> "}")

java-mode java-ts-mode

(p "System.out.print(" p ");")
(pf "System.out.printf(" p ");")
(pl "System.out.println(" p ");")

js-mode js-ts-mode tsx-ts-mode typescript-mode typescript-ts-mode web-mode

(afn "async " (i fn))
(ael (p "document") ".addEventListener('" (p "event") "', (" (p "ev") ") => {" n> r> n "});" >)
(ca "console.assert(" (p "assertion") ")")
(ce "console.error(" r ")")
(ci "console.log(`" r "`)")
(cl "console.log(" r ")")
(cv "console.log('" (p "var" var) ":', " var ")")
(cw "console.warn(" r ")")
(eac (p "el") ".appendChild(" r ");")
(erc (p "el") ".removeChild(" r ");")
(ev (p "collection") ".every((" (p "elem") ") => {" n> r> n "});" >)
(fd (p "collection") ".find((" (p "elem") ") => {" n> r> n "});" >)
(fe (p "collection") ".forEach((" (p "elem") ") => {" n> r> n "});" >)
(fl (p "collection") ".filter((" (p "elem") ") => {" n> r> n "});" >)
(fn "function " p "(" p ") {" n> r> n "}" >)
(forin "for (const " (p "key" key) " in " (p "object" obj) ") {" n>
       "if (" obj ".hasOwnProperty(" key ")) {" n> r> n "}" > n "}" >)
(forof "for (const " (p "iterator") " of " (p "collection") ") {" n> r> n "}" >)
(gc "const " (p "els") " = Array.from(" (p "document") ".getElementsByClassName('" r "'));")
(gi "const " (p "el") " = " (p "document") ".getElementById('" r "');")
(gt "const " (p "els") " = Array.from(" (p "document") ".getElementsByTagName('" r "'));")
(iife "(function (" p ") {" n> p n "})(" r ");")
(imp "import " (p "*") " from '" (p "module") "';")
(js "JSON.stringify(" r ")")
(jp "JSON.parse(" r ")")
(map (p "collection") ".map((" (p "elem") ") => {" n> r> n "});" >)
(new "const " (p "name") " = new " (p "Name") "(" r ");")
(oa "Object.assign(" (p "dest") ", " (p "source") ")")
(oc "Object.create(" r ")")
(oe "Object.entries(" r ")")
(ok "Object.keys(" r ")")
(ov "Object.values(" r ")")
(on (p "emitter") ".on('" (p "event") "', (" (p "args") ") => {" n> r> n "});" >)
(proto (p "Class") ".prototype." (p "method") " = (" (p "args") ") => {" n> r> n "}" >)
(qs "const " (p "el") " = " (p "document") ".querySelector('" r "');")
(qsa "const " (p "els") " = Array.from(" (p "document") ".querySelectorAll('" r "'));")
(rel (p "document") ".removeEventListener('" (p "event") "', '" (p "listener") "');")
(rd (p "collection") ".reduce((" (p "prev") ", " (p "curr") ") => {" n> r> n "});" >)
(rq "const " p " = require('" (r "module") "');")
(siv "setInterval(() => {" n> p n "}, " (p "interval") ");" >)
(sleep "await new Promise((r) => setTimeout(r, " (r "1000") "));")
(sto "setTimeout(() => {" n> p n "}, " (p "timeout") ");" >)
(sm (p "collection") ".some((" (p "elem") ") => {" n> r> n "});" >)
(try "try {" n> p n "} catch (err) {" > n> p n "} finally {" > n> r> n "}" >)

latex-mode

(abstract "\\begin{abstract}" n> r> n> "\\end{abstract}" >)
(align "\\begin{align}" n> r> n> "\\end{align}" >)
(alignn "\\begin{align*}" n> r> n> "\\end{align*}" >)
(gather "\\begin{gather}" n> r> n> "\\end{gather}" >)
(gatherr "\\begin{gather*}" n> r> n> "\\end{gather*}" >)
(appendix "\\begin{appendix}" n> r> n> "\\end{appendix}" >)
(begin "\\begin{" (s env) "}" n> r> n> "\\end{" (s env) "}" >)
(center "\\begin{center}" n> r> n> "\\end{center}" >)
(displaymath "\\begin{displaymath}" n> r> n> "\\end{displaymath}" >)
(document "\\begin{document}" n> r> n> "\\end{document}" >)
(enumerate "\\begin{enumerate}" n> "\\item " r> n> "\\end{enumerate}" >)
(equation "\\begin{equation}" n> r> n> "\\end{equation}" >)
(flushleft "\\begin{flushleft}" n> r> n> "\\end{flushleft}" >)
(flushright "\\begin{flushright}" n> r> n> "\\end{flushright}" >)
(frac "\\frac{" p "}{" q "}")
(fussypar "\\begin{fussypar}" r> n> "\\end{fussypar}" >)
(itemize "\\begin{itemize}" n> "\\item " r> n> "\\end{itemize}" >)
(letter "\\begin{letter}" n> r> n> "\\end{letter}" >)
(math "\\begin{math}" n> r> n> "\\end{math}" >)
(minipage "\\begin{minipage}[t]{0.5\linewidth}" n> r> n> "\\end{minipage}" >)
(quotation "\\begin{quotation}" n> r> n> "\\end{quotation}" >)
(quote "\\begin{quote}" n> r> n> "\\end{quote}" >)
(sloppypar "\\begin{sloppypar}" n> r> n> "\\end{sloppypar}" >)
(theindex "\\begin{theindex}" n> r> n> "\\end{theindex}" >)
(trivlist "\\begin{trivlist}" n> r> n> "\\end{trivlist}" >)
(verbatim "\\begin{verbatim}" n> r> n> "\\end{verbatim}" >)
(verbatimm "\\begin{verbatim*}" n> r> n> "\\end{verbatim*}" >)

lisp-mode

(ifl "(if-let (" p ")" n> r> ")")
(export "(:export " r ")")
(use "(:use " r ")")
(whenl "(when-let (" p ")" n> r> ")")

lua-mode lua-ts-mode

(elif "elseif " (p "cond") " then" n q >)
(fn "function " (p "name") "(" p ")" n q > n "end" >)
(for "for " (p "i") " = " p " do" n> q n "end" >)
(fore "for " (p "k") ", " (p "v") " in pairs(" (p "table") ") do" n> q n "end" >)
(if "if " (p "cond") " then" n q > n "end" >)
(l "local " q)
(lpcall "local " (p "status") ", " (p "err_or_value") " = " (i pcall))
(lreq (i l) (p "var") " = " (i req))
(p "print(" q ")")
(pcall "pcall(" (p "func") ")")
(req "require(\"" (p "module") "\")")
(self "function self:" (p "method") "(" p ")" n q > n "end" >)
(while "while " (p "cond") " do" n q > n "end" >)

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

(adc "#[allow(dead_code)]")
(afn "async " (i fn))
(afnr "async " (i fnr))
(afns "async " (i fns))
(afnw "async " (i fnw))
(allow "#[allow(" (p "lint") ")]")
(allowb "#![allow(" (p "lint") ")]")
(anfn "|" (p "arg") "| {" n> r> n "}" >)
(ar "#![allow(" (p "lint") ")]")
(ass "assert!(" (p "pred") ")")
(asseq "assert_eq!(" (p "expected") ", " (p "actual") ");")
(assne "assert_ne!(" (p "expected") ", " (p "actual") ");")
(case (p "pattern") " => " (p "expr"))
(cfg "#[cfg(" (p "opt") ")]")
(ct "#[cfg(\"test\")]")
(cv "#[cfg(" (p "opt") " = \"" (p "val") "\")]")
(d "dbg!(" r ")")
(deny "#[deny(" (p "lint") ")]")
(denyr "#![deny(" (p "lint") ")]")
(dr "#[derive(" (p "Trait") ")]")
(drf "#[derive(Default)]")
(drg "#[derive(Debug)]")
(drpe "#[derive(PartialEq)]")
(enum "enum " p " {" n> r> n> "}")
(ep "eprint!(" r ")")
(epl "eprintln!(" r ")")
(fn "fn " (p "name") "(" p ") {" n> r> n "}" >)
(fnr "fn " (p "name") "(" p ") -> " (p "()") " {" n> r> n "}" >)
(fnrw "fn " (p "name") "<" p ">(" p ") -> " (p "()") n>
      "where" n>
      (p "trait") n>
      " {" n> r> n "}" >)
(fns "fn " (p "name") "(&self) {" n> r> n "}" >)
(fnsr "fn " (p "name") "(&self) -> " (p "()") " {" n> r> n "}" >)
(fnw "fn " (p "name") "<" p ">(" p ")" n>
      "where" n>
      (p "trait") n>
      " {" n> r> n "}" >)
(fnza "fn " (p "name") "() {" n> r> n "}" >)
(for "for " (p "elem") " in " (p "iter") " {" n> r> n "}" >)
(ip "impl " p " {" n> r> n "}")
(ipb "impl Builder for " p " {" n>
     "fn default() -> Self {" n>
     r> n>
     "}" n> "}")
(ipd "impl Default for " p " {" n>
     "fn default() -> Self {" n>
     r> n>
     "}" n> "}")
(ipf "impl " p " for " p " {" n> r> n "}")
(ipfd "impl std::fmt::Display for " (p "Type" type) " {" n>
      "fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {" n>
      "write!(f, \"" type "\")" n>
      "}" n> "}")
(ippe "impl PartialEq for " p " {" n>
      "fn eq(&self, other: &Self) -> bool {" n>
      r> n>
      "}" n n>
      "fn ne(&self, other: &Self) -> bool {" n>
      "!self.eq(other)" n>
      "}" n>
      "}")
(if "if " p " {" n> r> n "}" >)
(ifl "if let " (p "pattern") " = " (p "expr") " {" n> r> n "}" >)
(loop "loop {" n> r> n "}" >)
(macro "macro_rules! " p " {" n> r> n> "}")
(main "fn main() {" n> r> n "}")
(match "match " (p "expr") " {" n> r> n "}" >)
(mod "mod " (p "module") " {" n> r> n "}" >)
(modt "#[cfg(test)]" n> "mod tests {" n> "use super::*;" n n> (i test) n "}" >)
(modu "mod " (p "module") ";")
(p "print!(" r ")")
(pafn "pub " (i afn))
(pafnr "pub " (i afnr))
(pc "panic!(" r ");")
(penum "pub " (i enum))
(pfn "pub " (i fn))
(pfnr "pub " (i fnr))
(pfns "pub " (i fns))
(pfnsr "pub " (i fnsr))
(pfnw "pub " (i fnw))
(pl "println!(" r ")")
(pmod "pub " (i mod))
(pst "pub " (i st))
(ptt "pub " (i tt))
(spawn "spawn(proc() {" n> r> n "});" >)
(st "struct " (p "Name") " {" n> r> n> "}")
(stt "struct " (p "Name") "(" (p "Type") ");")
(stu "struct " (p "Name") ";")
(t "todo!(" r ")")
(te "type " (p "Alias") " = " (p "Type") ";")
(test "#[test]" n> (i fnza) >)
(testi "#[test]" n> "#[ignore]" n> (i fnza))
(testp "#[test]" n> "#[should_panic]" n> (i fnza))
(tt "trait " p " {" n> r> n> "}")
(un "union " p " {" n> r> n "}")
(vec "vec![" r "]")
(warn "#[warn(" (p "lint") ")]")
(warnb "#![warn(" (p "lint") ")]")
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

tsx-ts-mode

(rc "function " (p (file-name-base (buffer-file-name))) "(" p ") {" n>
    "return (" n>
    "<>" n> r> n> "</>" n>
    ");" n>
    "}" >)
(uc "const " (p "cachedFn") " = useCallback(" (p "fn") ", [" r "]);")
(uct "const " p " = useContext(" r ");")
(ue "useEffect(() => {" n> "return " p ";" n "}, [" r "]);" >)
(um "const " (p "memoized") " = useMemo(() => " (p "rst") ", [" r "]);")
(ur "const [" (p "state") ", " (p "dispatch") "] = useReducer(" (p "reducer") ", " (p "args") ");")
(urf "const " (p "ref") " = useRef(" r ");")
(us "const [" (p "var" var) ", set" (upcase-initials var) "] = useState(" r ");")

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

zig-mode zig-ts-mode

(arr "[" (p "_") "]" (p "type") "{" r "}")
(case (p "pattern") " => " (p "expr"))
(const "const " (p "name") ": " (p "type") " = " r ";")
(enum "const " (p "Name") " = enum {" n> r> n "};" >)
(enumt "const " (p "Name") " = enum(" (p "type") ") {" n> r> n "};" >)
(err "const " (p "Name") "Error = error {" n> r> n "};" >)
(expect "const expect = @import(\"std\").testing.expect;")
(fn "fn " (p "name") "(" p ") {" n> r> n "}" >)
(fnr "fn " (p "name") "(" p ") " (p "void") " {" n> r> n "}" >)
(for "for (" p ") |" p "| {" n> r> n "}" >)
(gfn "fn " (p "name") "(comptime T: type, " p ") type {" n> r> n "}" >)
(if "if (" p ") {" n> r> n "}" >)
(ifn "inline " (i fn))
(imp "const " (p "name") " = @import(\"" (r "lib") "\");")
(main "pub fn main() void {" n> r> n "}")
(p "std.debug.print(\"" p "\", .{" r "});")
(std "const std = @import(\"std\");")
(struct "const " (p "Name") " = struct {" n> r> n "}" >)
(switch "switch (" p ") {" n> (i case) "," n> "else => " p "," n "}" >)
(te "if (" p ") " p " else " r)
(test "test \"" (p "name") "\" {" n> r> n "}" >)
(union "const " (p "Name") " = union {" n> r> n "};" >)
(var "var " (p "name") ": " (p "type") " = " r ";")
(while "while (" p ") {" n> r> n "}" >)
(whilei "while (" p ") : (" p ") {" n> r> n "}" >)

;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
