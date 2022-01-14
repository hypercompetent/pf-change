get_path_files <- function(path, file_type, recursive = FALSE) {
    pattern <- paste0(file_type, "$")
    files <- list.files(path, pattern = file_type, full.names = TRUE, recursive = recursive)
    files <- gsub("/+","/",files)
    files
}

get_project_folder_df <- function(project_folder) {
    message(paste("Retrieving file list for",project_folder))
    pf_files <- listFilesInProjectFolders(list(project_folder))$files
    pf_df <- map_dfr(pf_files, list_to_named_df, header_names = flattened_names(pf_files))
    pf_df %>%
      rowwise() %>%
      mutate(date_time = sub("/.+","",name)) %>%
      mutate(file_name = sub(paste0(date_time,"/"),"",name)) %>%
      mutate(path = ifelse(grepl(".tar.gz$", file_name),
                           sub(".tar.gz","/",file_name),
                           sub("[^/]+$","",file_name))
      )
}

pack_path_files <- function(path, file_type, recursive = FALSE) {
    if(file_type == "*") {
        tar_name <- paste0(sub("/$","",path), ".tar.gz")
        tar_command <- paste("tar -czf",
                             tar_name, 
                             path)
        system(tar_command)
    } else {
        pattern <- paste0(file_type, "$")
        path_files <- get_path_files(path, pattern, recursive)
        tar_name <- paste0(sub("/$","",path), ".tar.gz")
        tar_command <- paste("tar -czf",
                            tar_name, 
                            paste(path_files, collapse = " "))
        system(tar_command)
    }

}

unpack_path_files <- function(path) {
    tar_name <- paste0(sub("/$","",path), ".tar.gz")
    untar_command <- paste("tar -xzf", tar_name)
    system(untar_command)
    file.remove(tar_name)
}

retrieve_paths <- function(path_df, project_folder) {
    
    pf_df <- get_project_folder_df(project_folder)
    
    for(i in seq_along(1:nrow(path_df))) {

        tag_list <- stash_row_to_tag_list(path_df, i)
        
        path_pf_df <- pf_df %>%
              filter(path == path_df$path[i]) %>%
              filter(metadata.userTags.group == path_df$tag_group[i],
                     metadata.userTags.origin == path_df$tag_origin[i],
                     metadata.userTags.version == path_df$tag_version[i])
        
        if(nrow(path_pf_df) == 0) {
            stop(paste("Can't find stashed file", path_df$path[i], "with matching tags"))
        } else {
            message(paste("Found", path_df$path[i]))
        }
        
        if(!dir.exists(path_df$path[i])) {
            message(paste("Creating local", path_df$path[i]))
            dir.create(path_df$path[i], recursive = TRUE)
        }
        
        message(paste("Retrieving", path_df$path[i]))
        
        if(path_df$tar[i]) {
            temp_dir <- sub("[^/]+$","",path_pf_df$name[1])
            if(!dir.exists(temp_dir)) {
                dir.create(temp_dir, recursive = TRUE)
            }
            
            downloadFileFromProjectFolder(project_folder, path_pf_df$name[1])
            file.rename(path_pf_df$name[1], path_pf_df$file_name[1])
            unpack_path_files(path_pf_df$path[1])
            
            system(paste("rm -r",sub("/.+","",temp_dir)))
        } else {
            for(i in seq_along(1:nrow(path_pf_df))) {
                temp_dir <- sub("[^/]+$","",path_pf_df$name[i])
                if(!dir.exists(temp_dir)) {
                    dir.create(temp_dir, recursive = TRUE)
                }
                
                downloadFileFromProjectFolder(project_folder, path_pf_df$name[i])
                file.rename(path_pf_df$name[i], path_pf_df$file_name[i])
                
                system(paste("rm -r",sub("/.+","",temp_dir)))
            }
        }
    }
}

md5_file <- function(file_path) {
    md5_command <- paste0("md5sum ",file_path," | awk '{print $1}'")
    system(md5_command, intern = TRUE)
}

stash_paths <- function(path_df, watch_folder, project_folder, overwrite = FALSE) {
    
    pf_df <- get_project_folder_df(project_folder)
    
    for(i in seq_along(1:nrow(path_df))) {
        
        if(!dir.exists(path_df$path[i])) {
            message(paste("Can't find", path_df$path[i], ". Skipping."))
        }
        
        tag_list <- stash_row_to_tag_list(path_df, i)
        if(path_df$tar[i]) {
            message(paste("packing", path_df$path[i]))
            pack_path_files(path_df$path[i], path_df$file_type[i], recursive = path_df$recursive[i])
            stash_packed_path(path_df$path[i], 
                              project_folder = path_df$project_folder[i],
                              watch_folder = watch_folder,
                              tag_list = tag_list,
                              project_folder_df = pf_df,
                              overwrite = overwrite)
        } else {
            path_files <- get_path_files(path_df$path[i], path_df$file_type[i], recursive=path_df$recursive[i])
            
            if(length(path_files) == 0) {
                message(paste("Can't find ", path_df$file_type[i], "files in", path_df$path[i], ". Skipping."))
            }
            
            for(path_file in path_files) {
                stash_file(path_file, 
                           project_folder = path_df$project_folder[i],
                           watch_folder = watch_folder,
                           project_folder_df = pf_df,
                           tag_list = tag_list,
                           overwrite = overwrite)
            }
        }
    }
}

stash_row_to_tag_list <- function(stash_dirs, stash_row) {
    tag_names <- names(stash_dirs)[grepl("^tag_",names(stash_dirs))]
    tag_list <- as.list(unlist(stash_dirs[stash_row,tag_names]))
    names(tag_list) <- sub("^tag_","metadata.userTags.",names(tag_list))
    
    tag_list <- tag_list[!sapply(tag_list, is.na)]
    
    tag_list
}

check_stash_file_exists <- function(
    file_path, 
    project_folder, 
    project_folder_df = NULL, 
    tag_list) {
    
    tag_list[["metadata.userTags.name"]] <- file_path
    
    key_tags <- c(
        "metadata.userTags.name",
        "metadata.userTags.version"
    )
    
    tag_list <- tag_list[key_tags]
    names(tag_list) <- key_tags
    
    if(is.null(project_folder_df)) {
        project_folder_df <- get_project_folder_df(project_folder)
    }
    
    if(nrow(project_folder_df) == 0) {
        return(FALSE)
    }

    pf_df <- project_folder_df %>%
      filter(file_name == file_path)

    if(nrow(pf_df) == 0) {
        return(FALSE)
    } else {
        res <- map_lgl(
            1:nrow(pf_df),
            function(i) {
                pf <- pf_df[i,]
                tag_match <- map_lgl(
                    1:length(tag_list),
                    function(t) {
                        t_res <- pf[[names(tag_list)[t]]] == tag_list[[t]]
                        if(is.null(t_res)) {
                            FALSE
                        } else {
                            t_res
                        }
                    })
                all(tag_match)
            }
        )
        return(any(res))
    }
}

format_tag_list_for_hise <- function(tag_list) {
    names(tag_list) <- sub("metadata.userTags.","",names(tag_list))
    list(
        userTags = tag_list
    )
    
}

get_latest_stash_file <- function(file_path, project_folder) {
    pf_df <- get_project_folder_df(project_folder)
    pf_df <- pf_df %>%
      rowwise() %>%
      mutate(date_time = sub("/.+","",name)) %>%
      mutate(file_name = sub(paste0(date_time,"/"),"",name))
    latest_pf <- pf_df %>%
      filter(file_name == file_path) %>%
      arrange(desc(date_time)) %>%
      slice(1)
    latest_pf
}

tag_latest_stash_file <- function(file_path, project_folder, tag_list) {
    tag_list[["metadata.userTags.name"]] <- file_path

    tag_list <- format_tag_list_for_hise(tag_list)
    latest_pf <- get_latest_stash_file(file_path, project_folder)
    res <- setFileMetadataInProjectFolder(
        project_folder,
        latest_pf$name[1],
        fieldsToSet = tag_list
    )
    message(res$message)
}

stash_file <- function(file_path, 
                       project_folder,
                       watch_folder, 
                       tag_list = NULL,
                       project_folder_df = NULL,
                       overwrite = FALSE) {

    if(is.null(project_folder_df)) {
        project_folder_df <- get_project_folder_df(project_folder)
    }
    
    message("Computing md5sum")
    md5sum <- md5_file(file_path)
    
    exists <- check_stash_file_exists(
        file_path = file_path,
        project_folder = project_folder,
        project_folder_df = project_folder_df,
        tag_list = tag_list
    )
    
    tag_list[["metadata.userTags.other"]] <- paste0(tag_list[["metadata.userTags.other"]], ";md5sum:",md5sum)

    if(exists & !overwrite) {
        message(paste(file_path, "with matching tags already exists. Skipping"))
    } else {
        message(paste("uploading", file_path))
        gsutil_command <- paste0("gsutil cp ",
                                 file_path," ",
                                 watch_folder,file_path)
        message(gsutil_command)
        system(gsutil_command)
        message("pausing for ingest")
        Sys.sleep(20)
        message(paste("tagging", file_path))
        tag_latest_stash_file(file_path, project_folder, tag_list)
    }
}

stash_packed_path <- function(path,
                              project_folder,
                              watch_folder, 
                              tag_list = NULL,
                              project_folder_df = NULL,
                              overwrite = FALSE) {
    tar_name <- paste0(sub("/$","",path), ".tar.gz")
    
    if(is.null(project_folder_df)) {
        project_folder_df <- get_project_folder_df(project_folder)
    }
    
    stash_file(
        tar_name,
        project_folder,
        watch_folder,
        tag_list,
        project_folder_df,
        overwrite
    )
}

flattened_names <- function(in_list) {
    unique(unlist(map(in_list, function(l) {names(unlist(l))})))
}

list_to_named_df <- function(l, header_names) {
    l <- as.list(unlist(l))
    missing <- setdiff(header_names, names(l))
    if(length(missing) > 0) {
        for(m in missing) {
            l[[m]] <- ""
        }
    }
    df <- as.data.frame(l)
    df[,header_names]
}