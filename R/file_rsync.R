file_rsync <- function(fls, odir, opts = "-avP", cores = 1){
  pacman::p_load(parallel, glue)
  cmds = glue("rsync {opts} {fls} {odir}/")
  flog.debug(cmds)
  tmp = mclapply(cmds, system, mc.cores = cores, mc.preschedule = FALSE)
  tmp
}
