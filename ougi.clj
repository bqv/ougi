
(import '(java.net Socket HttpURLConnection URLEncoder)
        '(java.io BufferedWriter OutputStreamWriter BufferedReader InputStreamReader File)
        '(java.text Normalizer)
        '(java.util.regex Pattern)
        '(clojure.lang LineNumberingPushbackReader))
(use '[clojure.string :only [join split blank? escape replace]])
(use '[clojure.java.shell :only [sh]])
(use '[clojure.java.io :only [as-url]])

(defn now [] (int (/ (System/currentTimeMillis) 1000)))

(def nick "ougi")
(def autojoin (join "," []))
(def prefix "!")

(def servers ['("localhost" 6667 "ougi/subluminal:oshino") '("localhost" 6667 "ougi/freenode:oshino")])

(defn sock [shost sport] (new Socket shost sport))

(defn trunc [s c]
  (subs s 0 (min (count s) c)))

(defn gist [s cmd]
  (let [url (as-url "http://ix.io")
        output (str "Output of command: " cmd "\n=====\n\n" s)
        post (str "f:1=" (URLEncoder/encode output))
        conn (cast HttpURLConnection (.openConnection url))]
    (print post)
    (. conn (setDoOutput true))
    (. conn (setDoInput true))
    (. conn (setUseCaches false))
    (. conn (setRequestMethod "POST"))
    (. conn (setRequestProperty "Content-Type" "application/x-www-form-urlencoded"))
    (. conn (setRequestProperty "charset", "utf-8"))
    (. conn (setRequestProperty "Content-Length", (str (count (.getBytes post)))))
    (try 
      (let [null (spit (.getOutputStream conn) post)
            data (slurp (.getInputStream conn))]
        (.disconnect conn)
        (str "Output too long: see " data))
      (catch Exception e (do (print e) "Weird output. Failed to paste."))
    )))

(let [pyhdr "import sys,os,math,re,random;"
      pyftr ";#Hello "]
  (defn py [line sender]
    (let [code (str pyhdr line pyftr sender)
          dir (str "/tmp/" (now) "/")]
      (let [tmpdir (new File dir)]
        (if (not (.exists tmpdir)) (.mkdir tmpdir)))
      (spit (str dir "py.py") code)
      (let [ret (sh "pypy-sandbox" (str "--tmp=" dir) "--timeout=20" "--heapsize=64M" "py.py")
            out (:out ret)
            err (rest (split (:err ret) #"[\r\n]+"))]
        (sh "rm" "-rf" dir)
        (if (blank? out)
          (let [exc (second (reverse err))] 
            (if (blank? exc) "No output. (Remember to print)" (str "\u0002\u0002" exc)))
          (str "\u0002\u0002" (join " \u21b5 " (split (join "\\r" (split out #"\r")) #"\n"))))))))

(defn runsandbox ([cmd] (runsandbox cmd false)) 
  ([cmd net] (sh "timeout" "48" "/usr/sbin/open_init_pty" "sudo" "pflask" "-r" "/sandbox/jail" "-c" "/tmp" (if net "-N" "-n") "-u" "test" "-U" "timeout" "-sHUP" "-k32" "24" "bash" "-l" "-c" cmd)))

(defn rehash []
  (sh "sudo" "sh" "-c" "rm -rf /sandbox/tmp/*/*; umount /sandbox/jail; mount -t overlay -o upperdir=/sandbox/tmp/data/,lowerdir=/sandbox/pi/,workdir=/sandbox/tmp/work/ overlay /sandbox/jail/; chmod 777 /sandbox/jail/"))

(defn hoogle ([pat]
  (try
    (let [command (str "hoogle " pat)
          ret (runsandbox command false)
          out (:out ret)
          lines (split (:out ret) #"[\r\n\x00]+ *")]
      (conj
        (vec (map-indexed (comp #(replace % #"::" "::\u0002") #(str (+ 1 %1) ". \u0002" %2))
           (take 3 (filter #(.contains % "::") lines))))
        (gist out pat)))
    (catch Exception e (str "\u0002Hoogle error\u0002: " e)))))

(defn pointfree ([code]
  (try
    (let [command (str "pointfree --stdin <<EOF\n" code "\nEOF")
          ret (runsandbox command false)
          out (:out ret)
          lines (split (:out ret) #"[\r\n\x00]+ *")]
      (join " \u21b5" (butlast lines)))
    (catch Exception e (str "\u0002Pointfree error\u0002: " e)))))

(defn pointfreeio [code]
  (let [api "http://pointfree.io/snippet"
        query (str "code=" (URLEncoder/encode code))
        url (as-url (str api "?" query))
        conn (cast HttpURLConnection (.openConnection url))]
    (print query)
    (. conn (setDoOutput true))
    (. conn (setDoInput true))
    (. conn (setUseCaches false))
    (. conn (setRequestMethod "GET"))
    (. conn (setRequestProperty "charset", "utf-8"))
    (try 
      (let [data (slurp (.getInputStream conn))]
        (.disconnect conn)
        (read-string (re-find #"(?<=:)\".*\"(?=})" data)))
      (catch Exception e (do (print e) "Failed to pointfree."))
    )))

(defn sandbox ([cmd] (sandbox cmd false))
 ([cmd net]
  (try
    (let [command (str cmd)
          ret (runsandbox command net)
          exc (:exit ret)
          out (:out ret)
          lines (split (:out ret) #"[\r\n\x00]+ *")]
      (print out "\nEC: " exc "\n")
      (str "\u0002[\u0003" (if (= 0 exc) "03✔" "04✘") "\u0003]\u0002 " 
           (if (< (count out) 256)
             (trunc (join " \u21b5 " (butlast (map #(trunc % 500) lines))) 400 ) 
             (gist out cmd))))
    (catch Exception e (str "\u0002[WTF]\u0002 " e)))))

(defn bsandbox [interp code]
     (sandbox (str "cat <<'EOF' | (" interp ";echo) | ircize --remove\n" (replace code #"↵" "\n") "\nEOF")))

(defn curl [url]
  (let [host (second (re-matches #"^https?://([\w.]+)(/.*)?" url))]
    (if host
      (if (some #(= host %) ["gist.github.com","gist.githubusercontent.com"])
        (try
          (slurp url)
          (catch Exception e (str "\u0002[WTF]\u0002 " e)))
        (throw (Exception. "Forbidden URL")))
      (throw (Exception. "Only HTTP(s) allowed")))))

(defn wget [url sender]
  (try
    (let [data (curl url)
          name (str sender "_" (subs (str (now)) 7 10) ".txt")
          file (str "/sandbox/jail/tmp/" name)]
      (if (empty? data)
        "\u0002[\u000308?\u0003]\u0002 Failed: Empty file. Does the URL redirect?"
        (do
          (spit file data)
          (str "\u0002[\u000312➜\u0003]\u0002 Saved to /tmp/" name))))
    (catch Exception e (str "\u0002[WTF]\u0002 " e))))

(defn runcode [type code sender]
  (print "Running " type)
  (case (.toLowerCase type)
    ("pypy") (py code sender)
    ("wget" "curl") (wget code sender)
    ("bash" "sh") (sandbox (str code ";echo"))
    ("hs" "ghc" "ghci" "haskell") (bsandbox "env HOME=/tmp ghci -fno-warn-tabs -XRankNTypes -XScopedTypeVariables -v0 /usr/local/bin/BotPrelude.hs" (str "import BotPrelude\n" code))
    ("python" "py") (runcode (str type 3) code sender)
    ("python2" "py2") (bsandbox "python2" (str "import os,sys,math,re,random,numpy as np\n" code))
    ("python3" "py3") (bsandbox "python3" (str "import os,sys,math,re,random,numpy as np\n" code))
    ("clojure" "clj") (bsandbox "xargs -0 clojure -j '-Xmx256m' -e" code)
    ("perl" "pl" "p5") (bsandbox "(echo 'binmode STDOUT, \":utf8\";'; cat) | perl -Mv5.14" code)
    ("java") (bsandbox "xargs -0 java.sh" code)
    ("c#" "cs") (bsandbox "xargs -0 cs.sh" code)
    ("c++" "cpp") (bsandbox "xargs -0 cpp.sh" code)
    ("c") (bsandbox "xargs -0 c.sh" code)
    ("bc") (bsandbox "bc" code)
    ("dc") (bsandbox "dc" code)
    ("gcc") (bsandbox "xargs -0 gcc.sh" code)
    ("g++" "gpp") (bsandbox "xargs -0 gpp.sh" code)
    ("php" "phpisbad") (bsandbox "php" (str "<?php " code ";?>"))
    ("rust") (bsandbox "xargs -0 rust.sh" code)
    ("ruby" "rb") (bsandbox "ruby" code)
    ("lua") (bsandbox "lua" code)
    ("javascript" "js") (bsandbox "xargs -0 node -p" code)
    ("brainfuck" "bf") (bsandbox "xargs -0 brainfuck.sh" code)
    ("go") (bsandbox "xargs -0 go.sh" code)
    ("golfscript" "gs") (bsandbox "golfscript.rb" code)
    ("f#" "fs") (bsandbox "xargs -0 fs.sh" code)
    ("forth") (bsandbox "xargs -0 -I{} gforth -e {} -e '.s bye' \\;" code)
    ("factor" "fac") (bsandbox "xargs -0 -I{} factor-vm -e='USING: kernel math math.functions math.constants prettyprint sequences assocs monads stack-checker arrays fry locals ; IN: ougi {} .s clear'" code)
    ("perl6" "p6") (bsandbox "perl6 - 2>&1" code)
    ("scheme" "scm") (bsandbox "scm" code)
    ("racket" "rkt") (bsandbox "racket -r -" code)
    ("swift") (bsandbox "swift -" code)
    ("scala") (bsandbox "xargs -0 scala.sh" code)
    ("ocaml") (bsandbox "xargs -0 ocaml.sh" code)
    ("r" "R") (bsandbox "R --slave" code)
    ("julia" "jl") (bsandbox "env HOME=/tmp julia" code)
    ("erlang" "erl") (bsandbox "env HOME=/tmp erl | sed '1d;$d'" code)
    ("pascal" "pc") (bsandbox "xargs -0 fpc.sh" code)
    ("smalltalk" "st") (bsandbox "gst -g /dev/stdin" code)
    ("x86") (bsandbox "xargs -0 x86.sh" code)
    ("unlambda" "ul") (bsandbox "unlambda" code)
    ("purescript" "psc") (bsandbox "psc +RTS -N1 -qg -A128M -RTS /dev/stdin" code)
    ("prolog" "plg") (bsandbox "gprolog | tail -n +5" code)
    ("elixir" "ex" "exs") (bsandbox "xargs -0 elixir.sh" code)
    ("idris") (bsandbox "xargs -0 env HOME=/tmp idris /usr/local/bin/BotPrelude.idr -e | cat" code)
    ("mips") "\u0002[\u000308>_>\u0003]\u0002 Dev: Not implemented yet"
    ("z80") "\u0002[\u000308>_>\u0003]\u0002 Dev: Not implemented yet"
    ("arm") "\u0002[\u000308>_>\u0003]\u0002 Dev: Not implemented yet"
    ("tex" "latex") (sandbox (str "xargs -0 latex.sh <<'EOF'\n" code "\nEOF") true)
    ("help") "No"
    ("") "No language given. Perhaps you mean '?wget>' -- Available languages: bash, haskell, python2, python3, clojure, perl, java, c#, c++, c, php, javascript, brainfuck, rust, ruby, lua, go, ghostscript, forth, factor, perl6, scheme, racket, swift, scala(!), ocaml, R, bc, dc, julia, erlang, pascal, smalltalk, unlambda, purescript, idris, prolog, x86, ada(?), mips(?), z80(?), arm(?)"
    (str "Unknown language '" type "'. Are you lost? http://i.imgur.com/P1Tu4Fh.gif")))

(defn runclj [expr]
  (try 
    (format "%s" (load-string expr))
    (catch Exception e (str "Exception: " (.getMessage e)))))

(defn main [sock svpass]
            (let  [ out (BufferedWriter. (OutputStreamWriter. (.getOutputStream sock)))
                    in (BufferedReader. (InputStreamReader. (.getInputStream sock)))
                    sendraw (fn [s] (. out (write (str s "\r\n")))
                                    (println (str "<= " s)))
                    sflush (fn [] (.flush out))
                    parse (fn [sender chan command params admin]
                              (case command
                                "ping" (do (sendraw (str "NOTICE " chan " :Pong!")))
                                "pong" (do (sendraw (str "NOTICE " chan " :Ping!")))
                                "say" (if admin (do (sendraw (str "PRIVMSG " chan " :" params))) nil)
                                "notice" (if admin (do (sendraw (str "NOTICE " chan " :" params))) nil)
                                "raw" (if admin (do (sendraw (str params))) nil)
                                "join" (if admin (do (sendraw (str "JOIN " params))) nil)
                                "part" (if admin (do (sendraw (str "PART " params))) nil)
                                "quit" (if admin (if (= "" params)
                                                   (sendraw "QUIT :But was I ever really here?")
                                                   (sendraw (str "QUIT :" params))) nil)
                                "kill" (if admin (do (sendraw (str "NOTICE " chan " :x_x")) (sflush) (System/exit 0)) nil)
                                "rehash" (do (rehash) (sendraw (str "NOTICE " chan " :\u0002Done\u0002")))
                                (println (str sender " sent command " command " in " chan " with params " params)))) ]

        (sendraw (str "NICK " nick))
        (sendraw "USER ougi 8 * :Clojure IRC Bot")
        (if svpass (sendraw (str "PASS " svpass)))
        (sflush)

        (loop [rawline nil]
             (if (not (= nil rawline))
                 (let [line (split rawline #" " 3)]
                      (cond 
                        (= (first line) "PING")
                          (sendraw (str "PONG " (second line)))
                        (= (first line) "ERROR")
                          (. sock (close))
                        (= (second line) "001")
                          (do 
                              (sendraw (str "MODE " nick " +B"))
                              (sendraw (str "JOIN " autojoin)))
                        (= (second line) "INVITE")
                          (let [params (split (last line) #" :?" 2)]
                               (sendraw (str "JOIN " (second params))))
                        (= (second line) "KICK")
                          (let [params (split (last line) #" :?" 2)]
                               (sendraw (str "JOIN " (first params))))
                        (= (second line) "NOTICE")
                          (let [params (split (last line) #" :?" 2)
                                msg (second params)
                                chan (first params)
                                hostmask (last (split (first line) #":" 2))
                                sender (first (split hostmask #"[:!@]" 3))]
                              (println (str sender " sent notice " msg " to " chan)))
                        (= (second line) "PRIVMSG")
                          (let [params (split (last line) #" :?" 2)
                                msg (second params)
                                hostmask (last (split (first line) #":" 2))
                                sender (first (split hostmask #"[:!@]" 3))
                                chan (let [chan (first params)]
                                        (if (= \# (first chan)) chan sender))
                                earlrewrite (next (re-matches #"^\[([^\]]+)\] (.*)$" msg))]
                              (let [sender (if earlrewrite (first earlrewrite) sender)
                                    msg (if earlrewrite (second earlrewrite) msg)
                                    cmdline (split msg (re-pattern (str (Pattern/quote prefix) "(?! )")) 2)
                                    cljline (re-matches #"\?[\? ] ?(.*)" msg)
                                    haskline (re-matches #"\\ (.*)" msg)
                                    factrline (re-matches #": (.*)" msg)
                                    perlline (re-matches #"» (.*)" msg)
                                    erlline (re-matches #"- (.*)" msg)
                                    codeline (re-matches #"[\!\?]([\+\d\p{L}#]*)> ?(.*)" msg)
                                    chooseline (re-matches #"\.choose (.*)" msg)
                                    hoogline (re-matches #"[\.\!]hoog (.*)" msg)
                                    pfline (re-matches #"[\.\!]pf (.*)" msg)]
                                  (if (= "" (first cmdline))
                                      (let [command (split (second cmdline) #" " 2)
                                            admin   (or (= sender "me") (= sender "nitia"))]
                                          (parse sender chan (first command) (second command) admin))
                                      (println (join " " [sender "sent" msg "to" chan]))
                                      )
                                  (if (and cljline (or (= sender "me") (= sender "nitia")))
                                      (let [expr (second cljline)]
                                        (sendraw (str "NOTICE " chan " :\u0002\u0002" (runclj expr)))))
                                  (if (and haskline (not= "#cs-york" chan))
                                      (let [type "haskell"
                                            code (second haskline)]
                                        (future
                                          (sendraw (str "NOTICE " chan " :\u0002\u0002" (apply str (drop 10 (runcode type code sender)))))
                                          (sflush))))
                                  (if (and factrline (not= "#cs-york" chan))
                                      (let [type "factor"
                                            code (second factrline)]
                                        (future
                                          (sendraw (str "NOTICE " chan " :\u0002\u0002" (apply str (drop 10 (runcode type code sender)))))
                                          (sflush))))
                                  (if (and perlline (not= "#cs-york" chan))
                                      (let [type "perl6"
                                            code (second perlline)]
                                        (future
                                          (sendraw (str "NOTICE " chan " :\u0002\u0002" (apply str (drop 10 (runcode type code sender)))))
                                          (sflush))))
                                  (if (and erlline (not= "#cs-york" chan))
                                      (let [type "erlang"
                                            code (second erlline)]
                                        (future 
                                          (sendraw (str "NOTICE " chan " :\u0002\u0002" (apply str (drop 10 (runcode type code sender)))))
                                          (sflush))))
                                  (if codeline
                                      (let [type (second codeline)
                                            code (last codeline)]
                                        (future 
                                          (sendraw (str "NOTICE " chan " :" (runcode type code sender)))
                                          (sflush))))
                                  (if chooseline
                                      (let [choose (split (second chooseline) #" *, *")]
                                        (if (< 1 (.length choose))
                                          (sendraw (str "PRIVMSG " chan " :" sender ": \u200B" (rand-nth choose))))))
                                  (if hoogline
                                      (let [query (str "\"" (second hoogline) "\"")]
                                        (println (str hoogline))
                                        (doall (map #(sendraw (str "NOTICE " chan " :\u0002\u0002" %)) (hoogle query)))))
                                  (if pfline
                                      (let [query (second pfline)]
                                        (println (str pfline))
                                        (sendraw (str "NOTICE " chan " :\u0002\u0002" (pointfree query)))))))
                        :else
                          (println (str "=> " rawline))
                          )))
             (if (.isClosed sock)
                 nil
                 (do
                     (sflush)
                     (recur (.readLine in)))))))

(defn runserver [server]
    (let [ host (first server)
           port (second server)
           pass (last server) ]
        (future (main (sock host port) pass))))

(doall (map runserver servers))
