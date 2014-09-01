import System.Process
import Control.Concurrent
import Data.List

defMem = 0.7
defCpu = 0.7
defTme = "15:00:00"
defNic = -5
hostWidth = 15
knownUsers = []
groupsToCheck = ["linux-login-sys"]

data Async a = Async (MVar a)

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyMVar
  forkIO (do r <- action; putMVar var r)
  return (Async var)

wait :: Async a -> IO a
wait (Async var) = readMVar var

remoteCheck :: [Char] -> IO ()
remoteCheck host = do
     procs <- check host 
     let badProcs = case procs of
                          [] -> []
                          a  -> filterBadProcs $ tail $ lines a
         runaways = map ((++) (hostname ++ spaces)) badProcs
     mapM_ print runaways
     return ()
    where
     hostname = (takeWhile (/= '.') host) 
     spaces = replicate (14-(length hostname)) ' '

check :: String -> IO String
check host = do
    (_, procs, _) <- readProcessWithExitCode  "ssh" ["-oStrictHostKeyChecking=no", 
                                                    "-o ConnectTimeout=5", 
                                                    host,
                                                    "ps -eo ",
                                                    "user,ni,pid,ppid,pcpu,pmem,time,comm"] []
    return procs

filterBadProcs :: [[Char]] -> [[Char]]
filterBadProcs l = [bad | bad <- l, isBadProc bad]

isBadProc :: String -> Bool
isBadProc potentialRunaway = ((highMem) || (highCpu)) && (runningForAwhile) && (isntNice) && (notRoot) && notKnownUser
  where 
    (user:ni:pid:ppid:pcpu:pmem:time:com) = words potentialRunaway
    highMem = (read pmem::Float) > defMem
    highCpu = (read pcpu::Float) > defCpu
    runningForAwhile = time > defTme
    isntNice = (read ni::Int) > defNic 
    notRoot = user /= "root"
    notKnownUser = not $ elem user knownUsers

hosts :: IO [String]
hosts = do 
    str <- readProcess "netgrouplist" groupsToCheck []
    return (nub $ lines str)

main :: IO ()
main = do 
    putStrLn header
    hostlist <- hosts
    a <- mapM (async . remoteCheck) hostlist
    mapM_ wait a
  where 
    header = "HOST           USER      NI   PID  PPID %CPU %MEM     TIME COMMAND"
