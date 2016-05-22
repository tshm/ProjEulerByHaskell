import System.Environment (getArgs)
import Text.Regex
import Text.Regex.Posix

-- |
-- >>> glob "*7*" ["johab.py", "gen_probe.ko", "palmtx.h", "macpath.py", "tzp", "dm-dirty-log.h", "bh1770.h", "pktloc", "faillog.8.gz", "zconf.gperf"]
-- ["bh1770.h"]
--
-- >>> glob "*[0123456789]*[auoei]*" ["IBM1008_420.so","zfgrep","limits.conf.5.gz","tc.h","ilogb.3.gz","limits.conf","CyrAsia-TerminusBold28x14.psf.gz","nf_conntrack_sip.ko","DistUpgradeViewNonInteractive.pyc","NFKDQC"]
-- ["IBM1008_420.so"]
--
-- >>> glob "*.???" ["max_user_watches","arptables.h","net_namespace","Kannada.pl","menu_no_no.utf-8.vim","shtags.1","unistd_32_ia32.h","gettext-tools.mo","ntpdate.md5sums","linkat.2.gz"]
-- ["menu_no_no.utf-8.vim"]
--
-- >>> glob "*.pdf" ["OldItali.pl","term.log","plymouth-upstart-bridge","rand.so","libipw.ko","jisfreq.pyc","impedance-analyzer","xmon.h","1.5.0.3.txt","bank"]
-- []
--
-- >>> glob "g*.*" ["56b8a0b6.0","sl.vim","digctl.h","groff-base.conffiles","python-software-properties.md5sums","CountryInformation.py","use_zero_page","session-noninteractive","d2i_RSAPublicKey.3ssl.gz","container-detect.log.4.gz"]
-- ["groff-base.conffiles"]
--
-- >>> glob  "*[0123456789]*" ["keyboard.h","machinecheck","46b2fd3b.0","libip6t_frag.so","timer_defs.h","nano-menu.xpm","NI","vim-keys.conf","setjmp.h","memcg"]
-- ["46b2fd3b.0","libip6t_frag.so"]
--
glob :: String -> [String] -> [String]
glob p xs = filter (=~ regex) xs where
  regex = "^" ++ (concatMap conv p) ++ "$"
  conv c = case c of
    '.'  -> "\\."
    '?'  -> "."
    '*'  -> ".*"
    _    -> [c]

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . proc) $ lines doc where
    proc ln = if null xs'' then "-" else unwords xs'' where
      xs'' = glob pat xs'
      (pat:xs') = words ln

