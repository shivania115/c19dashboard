library(cronR)


cmd <- cron_rscript("/Users/poojanaik/Box Sync/COVID19_data_shared/DataUpload/RaceDataScript.R", rscript_log = sprintf("%s.log",
                    tools::file_path_sans_ext("/Users/poojanaik/Box Sync/COVID19_data_shared/DataUpload/RaceDataScript.R")), rscript_args = "",
                    cmd = file.path(Sys.getenv("R_HOME"), "bin", "Rscript"),
                    log_append = TRUE)

cron_add(command = cmd, frequency = 'daily', id = 'RaceDatadaily', description = 'RaceDataUpdate', at='10:15AM',tags = 'RaceData')


cron_ls()


