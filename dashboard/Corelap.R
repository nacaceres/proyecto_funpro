corelap <- function(dptos){
  #Paso 1: Definir la matriz de cercanías
  m_cerc = data.matrix(dptos[2:length((dptos))])
  dptos_n = colnames(m_cerc)
  dimnames(m_cerc) = list(dptos_n,dptos_n)
  
  #Paso 2: Selección del dpto con mayor TCR
  tcr_ini=rowSums(m_cerc)
  max_ini_tcr=max(tcr_ini)
  dpto_index=match(max_ini_tcr,tcr_ini)
  dpto_inicial=dptos_n[dpto_index]
  dptos_asginados = c(dpto_inicial)
  loc_mat = matrix("",nrow = (length(dptos_n)*2-1),ncol=(length(dptos_n)*2-1))
  loc_mat[length(dptos_n),length(dptos_n)]=dpto_inicial
  loc_list=list()
  loc_list[[dpto_inicial]] = paste(length(dptos_n),",",length(dptos_n), sep="")
    
  #Paso 5: Si hay departamentos sin ubicar, volver al paso 3.
  while(length(dptos_asginados)<length(dptos_n)){
    #Paso 3: Buscar el departamento no ubicado con TCR más alto con respecto a los dptos ya ubicados
    m_act=m_cerc[dptos_asginados,setdiff(dptos_n,dptos_asginados)]
    if(! is.null(nrow(m_act))){
      tcr_act=colSums(m_act)
    }
    else{
      tcr_act=m_act
    }
    max_act_tcr=max(tcr_act)
    dpto_act_index=match(max_act_tcr,tcr_act)
    if(! is.null(nrow(m_act))){
      dpto_act=colnames(m_act)[dpto_act_index]
    }
    else{
      dpto_act=setdiff(dptos_n,dptos_asginados)[dpto_act_index]
    }
    dptos_asginados = c(dptos_asginados,dpto_act)
    #Paso 4: Buscar la posición del dpto, de tal manera que se minimice la F.O
    fo_act_mat = matrix(0,nrow = (length(dptos_n)*2-1),ncol=(length(dptos_n)*2-1))
    for(i in 1:nrow(loc_mat)){
      for(j in 1:ncol(loc_mat)){
        if(is.na(match(paste(i,",",j, sep=""),loc_list))){
          fo_actual = 0
          for(k in 1:length(loc_list)){
            x_tar = as.numeric(strsplit(loc_list[[k]], ",")[[1]][1])
            y_tar = as.numeric(strsplit(loc_list[[k]], ",")[[1]][2])
            distancia = abs(i-x_tar) + abs(j-y_tar)
            cerc=m_cerc[names(loc_list)[k],dpto_act]
            fo_actual=fo_actual+distancia*cerc
          }
          fo_act_mat[i,j] = fo_actual
        }
        else{
          #No se pueden tomar espacios que ya estan ocupados.
          fo_act_mat[i,j]=99999999999999999999
        }
      }
    }
    coor = which(fo_act_mat == min(fo_act_mat), arr.ind = TRUE)
    loc_list[[dpto_act]] = paste(coor[[1,1]],",",coor[[1,2]], sep="")
    loc_mat[coor[[1,1]],coor[[1,2]]]=dpto_act
  }
  #Paso 6: Calculo de la función objetivo:
  m_dist = matrix(0,nrow = length(dptos_n),ncol=length(dptos_n))
  dimnames(m_dist) = list(dptos_n,dptos_n)
  for(i in 1:length(loc_list)){
    for(j in 1:length(loc_list)){
      x_1 = as.numeric(strsplit(loc_list[[i]], ",")[[1]][1])
      y_1 = as.numeric(strsplit(loc_list[[i]], ",")[[1]][2])
      x_2 = as.numeric(strsplit(loc_list[[j]], ",")[[1]][1])
      y_2 = as.numeric(strsplit(loc_list[[j]], ",")[[1]][2])
      distancia = abs(x_1-x_2) + abs(y_1-y_2)
      m_dist[names(loc_list)[i],names(loc_list)[j]] = distancia
    }
  }
  fo=0
  for(i in 1:nrow(m_dist)){
    for(j in 1:ncol(m_dist)){
      fo=fo+m_dist[[i,j]]*m_cerc[[i,j]]
    }
  }
  return(list(loc_mat=loc_mat,fo=fo))
}
