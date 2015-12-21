
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
(def pass "ougi:oshino")
(def port 6667)

(def server "localhost")
(def autojoin (join "," ["#test" "#bots"]))
(def prefix "!")

(if (first *command-line-args*)
  (do
    (def server "irc.sublumin.al")
    (def prefix "!")))

(def sock (new Socket server port))
(def out (BufferedWriter. (OutputStreamWriter. (.getOutputStream sock))))
(def in (BufferedReader. (InputStreamReader. (.getInputStream sock))))

(defn sendraw [s]
      (. out (write (str s "\r\n")))
      (println (str "<= " s)))
(defn sflush []
      (.flush out))

(sendraw (str "NICK " nick))
(sendraw "USER ougi 8 * :Clojure IRC Bot")
(if pass (sendraw (str "PASS " pass)))
(sflush)

(defn trunc [s c]
  (print s "\n")
  (subs s 0 (min (count s) c)))

(defn gist [s cmd]
  (let [url (as-url "http://sprunge.us")
        output (str "Output of command: " cmd "\n=====\n\n" s)
        post (str "sprunge=" (URLEncoder/encode output))
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
  ([cmd net] (sh "timeout" "20" "/usr/sbin/open_init_pty" "sudo" "pflask" "-r" "/sandbox/jail" "-c" "/tmp" (if net "-N" "-n") "-u" "test" "-U" "timeout" "-sHUP" "-k16" "10" "bash" "-l" "-c" cmd)))

(defn sandbox ([cmd] (sandbox cmd false))
 ([cmd net]
  (try
    (let [command (str cmd)
          ret (runsandbox command net)
          exc (:exit ret)
          out (:out ret)
          lines (split (:out ret) #"[\r\n\x00]+ *")]
      (str "\u0002[\u0003" (if (= 0 exc) "03✔" "04✘") "\u0003]\u0002 " 
           (if (< (count out) 512)
             (trunc (join " \u21b5 " (butlast (map #(trunc % 500) lines))) 400 ) 
             (gist out cmd))))
    (catch Exception e (str "\u0002[WTF]\u0002 " e)))))

(defn bsandbox [interp code]
     (sandbox (str "cat <<'EOF' | (" interp ";echo)\n" code "\nEOF")))

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
    ("bash" "sh") (sandbox code)
    ("hs" "ghc" "ghci" "haskell") (bsandbox "env HOME=/tmp ghci -v0" (str "import Data.Complex\n" code))
    ("python" "py") (runcode (str type 3) code sender)
    ("python2" "py2") (bsandbox "python2" (str "import os,sys,math,re,random\n" code))
    ("python3" "py3") (bsandbox "python3" (str "import os,sys,math,re,random\n" code))
    ("clojure" "clj") (bsandbox "xargs -0 clojure -j '-Xmx256m' -e" code)
    ("perl" "pl") (bsandbox "perl -Mv5.14" code)
    ("java") (bsandbox "xargs -0 java.sh" code)
    ("c#" "cs") (bsandbox "xargs -0 cs.sh" code)
    ("c++" "cpp") (bsandbox "xargs -0 cpp.sh" code)
    ("c") (bsandbox "xargs -0 c.sh" code)
    ("php") (bsandbox "php" (str "<?php " code ";?>"))
    ("rust") (bsandbox "xargs -0 rust.sh" code)
    ("ruby" "rb") (bsandbox "ruby" code)
    ("lua") (bsandbox "lua" code)
    ("javascript" "js") (bsandbox "xargs -0 node -p" code)
    ("brainfuck" "bf") (bsandbox "xargs -0 brainfuck.sh" code)
    ("go") (bsandbox "xargs -0 go.sh" code)
    ("forth") (bsandbox "xargs -0 -I{} gforth -e {} -e '.s bye' \\;" code)
    ("factor" "fac") (bsandbox "xargs -0 -I{} factor-vm -e='USING: kernel math math.functions prettyprint ; IN: ougi {} .s clear'" code)
    ("perl6" "p6") (bsandbox "perl6" code)
    ("scala") "\u0002[\u000308>_>\u0003]\u0002 Dev: Not implemented yet"
    ("idris") "\u0002[\u000308>_>\u0003]\u0002 Dev: Not implemented yet"
    ("tex" "latex") (sandbox (str "xargs -0 latex.sh <<'EOF'\n" code "\nEOF") true)
    ("") "No language given. Perhaps you mean '?wget>' -- Available languages: bash, haskell, python2, python3, clojure, perl, java, c++, c, php, javascript, brainfuck, rust, ruby, lua, go, forth, factor, perl6, scala(!), ocaml(!), idris(!), latex"
    (str "Unknown language '" type "'. Are you lost? http://i.imgur.com/P1Tu4Fh.gif")))

(defn runclj [expr]
  (try 
    (format "%s" (load-string expr))
    (catch Exception e (str "Exception: " (.getMessage e)))))

(defn parse [sender chan command params]
  (case command
    "ping" (do (sendraw (str "NOTICE " chan " :Pong!")))
    "pong" (do (sendraw (str "NOTICE " chan " :Ping!")))
    "say" (do (sendraw (str "PRIVMSG " chan " :" params)))
    "notice" (do (sendraw (str "NOTICE " chan " :" params)))
    "raw" (do (sendraw (str params)))
    "join" (do (sendraw (str "JOIN " params)))
    "part" (do (sendraw (str "PART " params)))
    "quit" (if (= "" params)
	       (sendraw "QUIT :But was I ever really here?")
	       (sendraw (str "QUIT :" params)))
    "kill" (do (sendraw (str "NOTICE " chan " :x_x")) (sflush) (System/exit 0))
    (println (str sender " sent command " command " in " chan " with params " params))))

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
		        chan (first params)
		        hostmask (last (split (first line) #":" 2))
		        sender (first (split hostmask #"[:!@]" 3))
		        cmdline (split msg (re-pattern (str (Pattern/quote prefix) "(?! )")) 2)
                        cljline (re-matches #"\?[\? ] ?(.*)" msg)
                        haskline (re-matches #"\\ (.*)" msg)
                        forthline (re-matches #": (.*)" msg)
                        codeline (re-matches #"[\!\?]([\+\d\p{L}#]*)> ?(.*)" msg)
                        chooseline (re-matches #"\.choose (.*)" msg)]
		      (if (and (= "" (first cmdline)) (= sender "me"))
			  (let [command (split (second cmdline) #" " 2)]
				(parse sender chan (first command) (second command)))
			  (println (join " " [sender "sent" msg "to" chan]))
                          )
                      (if (and cljline (= sender "me"))
                          (let [expr (second cljline)]
                            (sendraw (str "NOTICE " chan " :" (runclj expr)))))
                      (if haskline
                          (let [type "haskell"
                                code (second haskline)]
                            (sendraw (str "NOTICE " chan " :" (apply str (drop 10 (runcode type code sender)))))))
                      (if forthline
                          (let [type "forth"
                                code (second forthline)]
                            (sendraw (str "NOTICE " chan " :" (apply str (drop 10 (runcode type code sender)))))))
                      (if codeline
                          (let [type (second codeline)
                                code (last codeline)]
                            (future 
                              (sendraw (str "NOTICE " chan " :" (runcode type code sender)))
                              (sflush))))
                      (if chooseline
                          (let [choose (split (second chooseline) #" *, *")]
                            (if (< 1 (.length choose))
                              (sendraw (str "PRIVMSG " chan " :" sender ": \u200B" (rand-nth choose)))))))
		:else
		  (println (str "=> " rawline))
		  )))
     (if (.isClosed sock)
     	 nil
     	 (do
	     (sflush)
	     (recur (.readLine in)))))
