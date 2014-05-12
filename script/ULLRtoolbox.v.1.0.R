cat('',fill=TRUE)
cat('********************************************************************',fill=TRUE)
cat('        Toolbox made by Juan A. Hernandez Cabrera  ',fill=TRUE)
cat('        Departamento Psicobiologia y Metodologia' ,fill=TRUE)
cat('        FACULTAD DE PSICOLOGIA -UNIVERSIDAD DE LA LAGUNA ', fill=TRUE)
cat('		   Canary Islands - SPAIN',fill=TRUE)
cat('        https://sites.google.com/site/ullrtoolbox ', fill=TRUE)
cat('********************************************************************',fill=TRUE)
cat('',fill=TRUE)

#---------------------------------------------------------------------------
 nombres.var=function(dat=NA,alfabetico=FALSE){
	if(class(try(is.na(dat)))=='logical') {
		cat('',fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' nombres.var(datos)',fill=TRUE)
		cat(' nombres.var(datos, alfabetico=T)',fill=TRUE)
		cat('-------------,--------------------------------------------------------', fill=TRUE)
		cat(' Te permite conocer los nombres de las variables y la columna que	', fill=TRUE)
		cat(' ocupa cada una de ellas en la base de datos. De gran utilidad		', fill=TRUE)
		cat(' cuando tienes una base de datos con muchas variables y deseas		', fill=TRUE)
		cat(' rFealizar un analisis en particular con unas variables concretas.	', fill=TRUE)
		cat(' Si incluyes el argumento alfabetico=T te las dara ordenadas por	', fill=TRUE)
		cat(' orden alfabetico										', fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
	return('help')
	}
	nombres=data.frame(names(dat))
	variables=as.character(nombres[,1])
	nombres$numero=1:dim(dat)[2]
	names(nombres)=c('nombre','numero')
	nombres$tipo=NA
	for(i in 1:length(variables))
		nombres$tipo[i]=class(dat[,variables[i]])[1]
	if(alfabetico) nombres=nombres[order(nombres$nombre),]
 return(nombres)
 }
 
 nv=nombres.var
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
 dimension=function(datos){
	dat=datos
	clase=class(dat)
	if(clase =='data.frame') {tipo=1; size=dim(dat) }
	if(clase =='matrix') {tipo=2; size=dim(dat) }
	if(clase =='list') {tipo=3; size=length(dat) }
	if(clase =='numeric') {tipo=4; size =length(dat) }
	if(clase =='character') {tipo=5; size =length(dat) }
	if(tipo <= 2){
	 cat('',fill=TRUE)
	 cat('   El objeto es una matriz que pertenece a la clase',clase,fill=TRUE)
	 cat('   y tiene ',size[1],'filas y',size[2],'columnas (variables)',fill=TRUE)
	}
	if(tipo ==3){
	 cat('',fill=TRUE)
	 cat('   El objeto es una lista que pertenece a la clase',clase,fill=TRUE)
	 cat('   y tiene una longitud de',size,'registros',fill=TRUE)
	}
	if(tipo >=4){
	 cat('',fill=TRUE)
	 cat('   El objeto es un vector que pertenece a la clase',clase,fill=TRUE)
	 cat('   y tiene una longitud de',size,'registros',fill=TRUE)
	}
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# crea.cat.fnc('REGRESION MULTIPLE')
#---------------------------------------------------------------------------
 crea.cat.fnc=function(etiqueta, n=66){
  n.char=nchar(etiqueta)
  if(n.char > 66) n=n.char+10
    guiones=paste(rep('-',n),'',sep='', collapse='')
    espacios=paste(rep(' ',(n-n.char)/2),'',sep='', collapse='')
    cat('',fill=TRUE)
    cat(paste('#',guiones,sep=''),fill=TRUE)
    cat(paste('#',espacios,etiqueta,espacios,sep=''),fill=TRUE)
    cat(paste('#',guiones,sep=''),fill=TRUE)
    cat('',fill=TRUE)
}
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# A partir de una data que solo tiene condi, genera los factores que la
# componen
#---------------------------------------------------------------------------
 crea.factores.desde.condi.fnc=function(datos,factores){
	dat=datos
	new.fac=data.frame(do.call(rbind,strsplit(as.character(dat$condi),'[:.:]')))
	names(new.fac)=factores
	dat_=cbind(dat,new.fac)
 return(dat_)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#
#---------------------------------------------------------------------------
 argumentos=function(funci) {
    cat('',fill=TRUE)
    cat('*** Estos son los argumentos que requiere la funcion:',fill=TRUE)
    cat('***                     ',funci,fill=TRUE)
#    cat('*** Si alguno es NA significa que el usuario no esta obligado a incluirlo', fill=TRUE)
#    cat('*** en la llamada a la funcion, ya que se utilizara un valor por defecto',fill=TRUE)
#    cat('*** que el usuario puede modificar en cualquier momento',fill=TRUE)
#    cat('',fill=TRUE)
#    cat('          ARGUMENTOS:',fill=TRUE); cat('',fill=TRUE)
	f1=as.list(formals(funci))
	f1=lapply(f1,as.character)
	argu=do.call(rbind,f1)
	argu=as.matrix(argu[,1])
 	dimnames(argu)=list(argumentos=row.names(argu), 'valor')
 	hay.dat=match('datos', row.names(argu))
 	if(!is.na(hay.dat[1])) argu[1]='datos'
 return(argu)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#
#---------------------------------------------------------------------------
 exporta.txt.fnc=function(datos=NA,nombre,dec=',', nombre.filas=FALSE, separador='tab',
						nombresvar=TRUE, miss='', csv=FALSE){
	if(class(try(is.na(datos)))=='logical') {
		cat('',fill=TRUE)
		crea.cat.fnc('exporta.txt.fnc')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat("      exporta.txt.fnc(mis.datos, nombre='datos', dec='.')			",fill=TRUE)
		cat("      exporta.txt.fnc(mis.datos, nombre='datos', dec='.', mis=-999)  ",fill=TRUE)
		cat("      exporta.txt.fnc(mis.datos, 'datos', nombres.filas=T)			",fill=TRUE)
		cat("      exporta.txt.fnc(mis.datos, 'datos', nombres.filas=T, csv=T)	",fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Guarda en un archivo externo de nombre datos.txt el objeto mis.datos	', fill=TRUE)
		cat(' El argumento dec (separador decimal) es por defecto el caracter coma.', fill=TRUE)
		cat(' El usuario puede indicar entre apostrofes cualquier otro separador.	', fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
	return('help')
	}
	if(csv) archivo=paste(nombre,'.csv',sep='') else archivo=paste(nombre,'.txt',sep='')
	if(separador=='tab' | separador=='tabulador') separador='\t'
	if(separador=='' | separador==' ') separador=' '

	write.table(datos,file=archivo,row.names=nombre.filas,col.names=nombresvar,
		sep=separador, dec=dec, na=miss, quote =FALSE)
 	cat('*** Se ha guardado el archivo: ',archivo,' ***',fill=TRUE)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
 centra.variable.fnc=function(datos,variable,silente=FALSE, que.factor=NA){
	dat=datos
    if(!silente) crea.cat.fnc('CENTRADO DE VARIABLE')
    nombres=names(dat)
    if(is.numeric(variable)) variable=nombres[variable]
    if(length(variable) > 1){
	cat('',fill=TRUE)
	cat('*** Error. La funcion no puede centrar mas de una variable.         ***',fill=TRUE)
	cat('*** Llama a la funcion tantas veces como variables quieras centrar. ***',fill=TRUE)
	cat('',fill=TRUE)
	stop( )
    }
    # Si el centrado es de gran media o por nivel jerarquico 
    if(is.na(que.factor[1])){
	if(!silente){
		new.var=paste('c.',variable,sep='')
		dat[,new.var]=dat[,variable]-mean(dat[,variable],na.rm=TRUE)
		cat('*** Se ha centrado con exito la variable',variable,'.El centrado',fill=TRUE)
		cat('*** se ha realizado sobre la gran media. Si deseas un centrado',fill=TRUE)
		cat('*** en cada nivel de un determinado factor, incluye el argumento',fill=TRUE)
		cat("*** que.factor. Ej: que.factor='School'",fill=TRUE)
		cat('',fill=TRUE)
		cat('*** Resumen de la variable centrada',new.var,fill=TRUE)
		print(summary(dat[,new.var]))
	}else{
		new.var=paste('c.',variable,sep='')
		dat[,new.var]=dat[,variable]-mean(dat[,variable],na.rm=TRUE)
	}
    }else{
	if(is.numeric(que.factor)){
	  cat('',fill=TRUE)
	  cat('*** Error. La variable incluida en que factor debe ser su nombre y no  ***',fill=TRUE)
	  cat('*** su numero de columna. Repite la llamada a la funcion incluyendo el ***',fill=TRUE)
	  cat("*** nombre y no el numero del factor. Ej. que.factor='sujeto'          ***",fill=TRUE)
	  cat('',fill=TRUE)
	  stop( )
	}
	dat$indice=1:dim(dat)[1]
	new.var=paste('c.',variable,sep='')
  miss=FALSE    
  suma=sum(frecuencias.fnc(dat,variable=que.factor,silente=TRUE)[[1]]$tabla) 
  if(suma !=dim(dat)[1]){
    miss=TRUE
    dat[,que.factor]=recode(dat[,que.factor],"NA='miss'")
  }  
	x.factor=divide.por.factor.fnc(dat, que.factor=que.factor, silente=TRUE)
	x.factor=lapply(x.factor, function(x) {
	      x[,new.var]=x[,variable]-mean(x[,variable],na.rm=TRUE)
	      return(x)})
	x.factor=data.frame(do.call(rbind,x.factor))
	x.factor=ordena.por.variable.fnc(x.factor, variable='indice', silente=TRUE)
	row.names(x.factor)=row.names(dat)
  if(miss) x.factor[,que.factor]=recode(x.factor[,que.factor]," 'miss'=NA")
      
	cat('*** Se ha centrado con exito la variable',variable,'en cada nivel',fill=TRUE)
	cat('*** de la variable',que.factor,'.La nueva variable centrada es:',new.var,fill=TRUE)
	cat('',fill=TRUE)
	dat=x.factor
     }
     # Fin de centrado
 return(dat)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#
#---------------------------------------------------------------------------
 comprueba.paquetes.instalados.fnc=function( ){
  version=as.numeric(R.version$major)
  if(version < 3){
    cat('',fill=TRUE)
    cat('--------------------------------------------------------------------------------',fill=TRUE)    
    cat('*** Error. Tienes una version de R anticuada. Las nuevas funciones incorporadas',fill=TRUE)
    cat('*** al toolbox requieren de R 3.0 o superior. Instala la ultima version de R ',fill=TRUE)
    cat('*** siguiendo las siguientes directrices: ',fill=TRUE)
    cat('',fill=TRUE)
    cat('*** Wind y Mac: https://sites.google.com/site/ullrtoolbox/home/como-empezar',fill=TRUE)
    cat('*** LINUX:      https://sites.google.com/site/ullrtoolbox/home/instalacion-linux-ubuntu',fill=TRUE)
    cat('',fill=TRUE)
    cat('                         *** MUY IMPORTANTE ***',fill=TRUE)
    cat('***  UNA VEZ INSTALADO DEBES EJECUTARLO LA PRIMERA VEZ COMO ADMINISTRADOR  ***',fill=TRUE)
    cat('***  PARA ASEGURAR LA CORRECTA INSTALACION DE LAS LIBRERIAS.               ***',fill=TRUE)
    cat('--------------------------------------------------------------------------------',fill=TRUE)    
    cat('',fill=TRUE)
    stop( )
  } 
  instalados=installed.packages()[,1]
  paquetes=c('MASS','nnet','car',
	'KernSmooth','gtools','gdata','bitops','caTools','gplots',
	'lattice','Matrix','codetools','Rcpp','RcppEigen','nlme','lme4',
	'leaps','lars','ElemStatLearn','bestglm',
	'ROCR',
	'foreign',
	'klaR',
	'GPArotation',
	'psych',
	'sfsmisc','survival','msm','mvtnorm','polycor','ltm',
	'ppcor',
	'ca',
	'rpart','mice',
	'colorspace','qvcalc','relimp','vcd','gnm','vcdExtra',
	'heplots','candisc',
	'polycor',
	'corpcor',
	'boot','survey','mitools','relaimpo',
	'mnormt','cluster','pbivnorm','quadprog','matrixcalc','Formula','ellipse','lavaan','sem','plyr','Hmisc', 	'igraph','jpeg','png','qgraph',
	'xtable',
	'zoo','lpSolve','sandwich','mediation')

  n.paq=length(paquetes)
  n.paq
  chivato=paquetes %in% instalados
  if(sum(chivato) < n.paq){
    call('*** Se instalaran las librerias. Selecciona el repositorio mas cercano ***', fill=TRUE)
    for(i in paquetes) install.packages(i)
  }
  
  instalados=installed.packages()[,1]
  chivato.new='sjPlot' %in% instalados
  if(!chivato.new){
    pack.sjPlot=c('digest', 'gtable', 'proto', 'stringr', 
                'RColorBrewer', 'dichromat', 'munsell', 
                'labeling', 'ggplot2', 'lmtest', 'reshape2', 
                'scales','sjPlot')
    for(i in pack.sjPlot) install.packages(i)
  } 

  instalados=installed.packages()[,1]
  chivato.new='likert' %in% instalados
  if(!chivato.new){
    pack.likert=c('gridExtra','reshape','likert')
    for(i in pack.likert) install.packages(i)
  }  

  instalados=installed.packages()[,1]
  chivato.new='lmerTest' %in% instalados
  if(!chivato.new){
    pack.lmerT=c('numDeriv','pbkrtest','lmerTest')
    for(i in pack.lmerT) install.packages(i)
  } 
  
  instalados=installed.packages()[,1]
  chivato.new='irr' %in% instalados
  if(!chivato.new){
    pack.irr='irr'
    for(i in pack.irr) install.packages(i)
  } 

  instalados=installed.packages()[,1]
  chivato.new='brglm' %in% instalados
  if(!chivato.new){
    pack.brglm='brglm'
    for(i in pack.brglm) install.packages(i)
  }   
  
  cat('*** Todas las librerias necesarias estan correctamente instaladas ***',fill=TRUE)
  cat('',fill=TRUE)
 }  
#---------------------------------------------------------------------------
 comprueba.paquetes.instalados.fnc( )
#---------------------------------------------------------------------------
# Instala las librerias necesarias para la ejecucion del toolbox
#---------------------------------------------------------------------------
 instala.librerias.fnc=function( ){
 	install.packages(c('car','lme4','bestglm','klaR','ROCR','psych','GPArotation'))
 }
#---------------------------------------------------------------------------
 
 data(package='psych',iqitems); data(package='psych',bfi); data(package='car',OBrienKaiser)
 data(package='mediation',jobs); data(package='mediation',framing); data(package='mediation',student)
 data(package='mice',nhanes); data(package='mice',nhanes2); data(package='bestglm',zprostate);
 data(package='irr',anxiety); ansiedad=anxiety; rm(anxiety)
 data(package='likert',pisaitems); pisaitems=pisaitems[,1:12]
 
#---------------------------------------------------------------------------
# histograma.fnc(datos, vd='tiempo', factores=c('diagnostico','curso'))
#---------------------------------------------------------------------------
 histograma.fnc=function(datos=NA,vd=NA,que.factor=NA,ylim=NA,orden=NA, check=FALSE, to.pdf=FALSE,
 						      variables=NA, p.tipica=FALSE, cortes=NA){
	if(class(try(is.na(datos)))=='logical') {
		cat('',fill=TRUE)
		crea.cat.fnc('histograma.fnc')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat("  histograma.fnc(datos, vd='tr')                                        ",fill=TRUE)
		cat("  histograma.fnc(datos, vd='tr', p.tipica=T)                            ",fill=TRUE)
		cat("  histograma.fnc(datos, vd='tr', p.tipica=T, cortes=c(-2,2) )           ",fill=TRUE)
		cat("  histograma.fnc(datos, vd='tr', que.factor='tratamiento')              ",fill=TRUE)
		cat("  histograma.fnc(datos, vd='tr', que.factor='tratamiento', orden=c(3,1))",fill=TRUE)
		cat("  histograma.fnc(datos, vd='tr', que.factor='tratamiento', check=T)     ",fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Genera histograma o histogramas suavizados para la variable indicada   ', fill=TRUE)
		cat(' en el argumento vd en cada nivel del factor incluido en el argumento', fill=TRUE)
		cat(' que factor. Si se incluye el argumento check=T este histograma se	', fill=TRUE)
		cat(' realizara en una sola ventana grafica donde se solaparan o no las	', fill=TRUE)
		cat(' curvas por cada nivel del factor explicitado en que.factor.		', fill=TRUE)
		cat(" ",fill=TRUE)
		cat("             https://sites.google.com/site/ullrtoolbox			", fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		print(argumentos('histograma.fnc'))
	return('help')
	}

	dat=datos
	check.que.factor(que.factor)

	require(lattice,quietly =TRUE)
	if(!is.na(vd[1])){
		if(is.numeric(vd)){
			nombres=names(datos)
			vd=nombres[vd]
			if(is.na(vd[1])){
			cat('',fill=TRUE)
			cat('*** Error. La variable dependiente introducida no existe en la base de datos  ***',fill=TRUE)
			cat('',fill=TRUE)
			stop( )
			}
		}
	}

	# SI NA VD Y DATOS APILADOS
	if(is.na(vd[1])){
		existe=match('vd', names(dat))
		if(!is.na(existe[1])){
 		  vd='vd'
		}else{
		  cat('',fill=TRUE)
		  cat('*** Error. Tus datos no estan apilados y no has incluido el argumento vd ***',fill=TRUE)
		 stop( )
		}
	} # FIN SI NA VD

	# CHEQUEA NOMBRE DE VD
	nombres=names(dat)
	n.col.vd=match(vd,nombres)
	if(is.na(n.col.vd[1])) {
		cat('',fill=TRUE)
		cat('*** Error. La variable dependiente',vd,'no existe en la base de datos  ***',fill=TRUE)
		cat('*** introducida. Estas son las variables disponibles:                  ***',fill=TRUE)
		cat('',fill=TRUE)
		print(nombres.var(dat))
	 stop( )
	}
	if(!to.pdf) x11()
	vd.ori='vd'
	if(nombres[n.col.vd]!='vd'){
		vd.ori=nombres[n.col.vd]
		nombres[n.col.vd]='vd'
		names(dat)=nombres
	} # FIN CHECK VD

	#
	if(p.tipica){
		dat=p.tipica.fnc(dat, variable='vd', silente=TRUE)
		dat=cambia.nombre.var.fnc(dat, c('vd','z.vd'),c('old.vd','vd'), silente=T)
	}
	if(!is.na(cortes[1])){
		dat=dat[dat$vd >=cortes[1] & dat$vd <= cortes[2],]
	}

   # SI NO HAY FACTORES
   if(is.na(que.factor[1])){
  		print(densityplot(~ vd, xlab=vd.ori ,
			main=vd.ori, data=dat))

   # SI FACTORES
   }else{
	hay.punto=grep(':',que.factor)
	if(length(hay.punto)!=0){
	  que.factores=strsplit(que.factor, ':')[[1]]
	  n.fac=length(que.factores)
	  if(n.fac==2) dat$condi=paste(dat[,que.factores[1]],dat[,que.factores[2]],sep='.')
	  if(n.fac==3) dat$condi=paste(dat[,que.factores[1]],dat[,que.factores[2]],dat[,que.factores[3]],sep='.')
	  que.factores='condi'
	  n.fac=1
	  que.factor='condi'
	  nombres=names(dat)
	}else{
	  que.factores=que.factor
	  n.fac=length(que.factores)
	}
	chivato.factores=match(que.factores,nombres)
	if(sum(is.na(chivato.factores))!=0){
		no.existe=que.factor[chivato.factores %in% NA]
		cat('',fill=TRUE)
		cat('*** Error. El siguiente factor:',no.existe,'no existe en la base de datos ***',fill=TRUE)
		stop( )
	}

 	if(n.fac > 3) {
 		cat('--------------------------------------------------------------------',
 			fill=TRUE)
 		cat('*** El numero maximo de factores a graficar simultaneamente es 3 ***',
 			fill=TRUE)
  		cat('*** Solicita nuevamente el grafico, eliminando algun factor      ***',
 			fill=TRUE)
 		cat('--------------------------------------------------------------------',
 			fill=TRUE)
 	}
 	if(n.fac==1){
	 if(!check){
 		if(is.na(orden[1])){
		     if(is.na(ylim[1])){
  			print(densityplot(~ vd | eval(parse(text=que.factor[1])),
  			xlab=vd.ori , main=paste('Histograma de ',vd.ori,' en (',que.factor[1],')',sep=''),data=dat))
		     }else{
  			print(densityplot(~ vd | eval(parse(text=que.factor[1])),
  			ylim=ylim,
			xlab=vd.ori,main=paste('Histograma de ',vd.ori,' en (',que.factor[1],')',sep=''),data=dat))
                     }
	 	}else{
		     if(is.na(ylim[1])){
	 		print(densityplot(~ vd  | eval(parse(text=que.factor[1])),
			xlab=vd.ori,layout=orden,main=paste(vd,' en (',que.factor[1],')',sep=''),data=dat))
		     }else{
	 		print(densityplot(~ vd  | eval(parse(text=que.factor[1])),
			xlab=vd.ori,layout=orden, ylim=ylim,
	 		main=paste('Histograma de ',vd.ori,' en (',que.factor[1],')',sep=''),data=dat))
		     }
	 	}
	# SI CHECK
	}else{
		dat[,que.factor]=factor(as.character(dat[,que.factor]))
		x.factor=split(dat,dat[,que.factor])
		densi=lapply(x.factor, function(x) density(na.omit(x[,'vd'])))
		max.y=max(do.call(rbind,lapply(densi,function(y) max(y$y))))
		limites=lapply(x.factor, function(x)
			c(min(na.omit(x[,'vd'])),max(na.omit(x[,'vd']))))
		medias=descriptivos.fnc(dat, vd='vd', que.factor=que.factor, silente=T)[[1]][1,]
		#medias=do.call(cbind,lapply(densi, function(x) mean(x$x)))

		limites=do.call(rbind,limites)
		limites=c(min(limites[,1]),max(limites[,2]))
 		plot(densi[[1]],xlim=limites,ylim=c(0,max.y),col='blue',main='',xlab='')
		title(paste(vd,' en cada nivel de ',que.factor,sep=''))
			for(i in 1:length(densi)){
				mtext(names(x.factor)[i],col=i,side=4,line=-i)
		 		lines(densi[[i]],col=i,lwd=2)
				cociente=i/2
				if(cociente-ceiling(cociente)!=0) entero=FALSE else entero=TRUE
				if(entero){
					rug(jitter(x.factor[[i]][,'vd']),col=i,side=3)
				}else{
					rug(jitter(x.factor[[i]][,'vd']),col=i)
				}
			}
			abline(v=medias,col=1:length(densi),lwd=2,lty=2)
	} # FIN SI CHECK

	} # FIN 1 FACTOR
 	if(n.fac==2){
 		 if(is.na(orden[1])){
			if(is.na(ylim[1])){
			print(densityplot(~ vd  | eval(parse(text=que.factor[1])),
			group=eval(parse(text=que.factor[2])),
			key = simpleKey(text = levels(dat[,que.factor[2]]),
			space = "right", points = TRUE),
			xlab=vd.ori,
	 		main=paste('Histograma de ',vd.ori,' en (',que.factor[1],':',que.factor[2],')',sep=''),data=dat))
			}else{
			print(densityplot(~ vd  | eval(parse(text=que.factor[1])),
			group=eval(parse(text=que.factor[2])),ylim=ylim,
			key = simpleKey(text = levels(dat[,que.factor[2]]),
			space = "right", points = TRUE),
			xlab=vd.ori,
	 		main=paste('Histograma de ',vd.ori,' en (',que.factor[1],':',que.factor[2],')',sep=''),data=dat))
			}
		}else{
              if(is.na(ylim[1])){
 			 print(densityplot(~ vd  | eval(parse(text=que.factor[1])),
			 group=eval(parse(text=que.factor[2])),
			 key = simpleKey(text = levels(dat[,que.factor[2]]),
			 space = "right", points = TRUE),
			 xlab=vd.ori,layout=orden,
	 		 main=paste('Histograma de ',vd.ori,' en (',que.factor[1],':',que.factor[2],')',sep=''),data=dat))
			}else{
 			 print(densityplot(~ vd  | eval(parse(text=que.factor[1])),
			 group=eval(parse(text=que.factor[2])),  ylim=ylim,
			 key = simpleKey(text = levels(dat[,que.factor[2]]),
			 space = "right", points = TRUE),
			 xlab=vd.ori,layout=orden,
	 		 main=paste('Histograma de ',vd.ori,' en (',que.factor[1],':',que.factor[2],')',sep=''),data=dat))
			}
		}
	} # FIN DOS FACTORES
 	if(n.fac==3){
		 que.factor=que.factores
 		 dat.sp=split(dat,dat[,que.factor[3]])
 		 dens=density(dat[,'vd'],na.rm=TRUE)$y
 		 limites=c(min(dens),max(dens))
 		 if(is.na(orden[1])){
 			lapply(dat.sp, function(x) {
 			X11()
 			print(densityplot(~ vd  | eval(parse(text=que.factor[1])),
			group=eval(parse(text=que.factor[2])),
			key = simpleKey(text = levels(dat[,que.factor[2]]),
			space = "right", points = TRUE),
			xlab=vd.ori,
	 		main=paste('Histograma de ',vd.ori,' en (',que.factor[1],':',que.factor[2],':',
	 			que.factor[3],'=',unique(as.character(x[,que.factor[3]])),')',sep=''),
	 			data=x))
	 		})
		}else{
 			lapply(dat.sp, function(x) {
 			X11()
 			print(densityplot(~ vd  | eval(parse(text=que.factor[1])),
			group=eval(parse(text=que.factor[2])),
			key = simpleKey(text = levels(dat[,que.factor[2]]),
			space = "right", points = TRUE),
			xlab=vd.ori,layout=orden,
	 		main=paste('Histograma de ',vd.ori,' en (',que.factor[1],':',que.factor[2],':',
	 			que.factor[3],'=',unique(as.character(x[,que.factor[3]])),')',sep=''),
	 			data=x))
	 		})
		}
	} # FIN 3 FACTORES
   } # FIN N. DE FACTORES
   
 }
#---------------------------------------------------------------------------
#
#---------------------------------------------------------------------------
 lee.archivo.externo.fnc=function(archivo=NA,hay.nombres=NA,decimal=NA,
	miss=NA,separador=NA,traspuesta=NA,spss=NA,etiquetas=TRUE,excel=FALSE,
	que.hoja=NA,silente=NA){
		cat('',fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat('*** La funcion lee.archivo.externo.fnc ha sido sustituida por:       ', fill=TRUE)
		cat('*** lee.archivo.fnc                                                  ', fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat('',fill=TRUE)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# lee.archivo.fnc('nombre.archivo')
#---------------------------------------------------------------------------
 lee.archivo.fnc=function(archivo=NA,hay.nombres=NA,decimal=NA,
		miss=NA,separador=NA,traspuesta=NA,spss=NA,etiquetas=TRUE,excel=FALSE,
		que.hoja=1,silente=NA, salta=0, nfilas=-1){
	if(is.na(archivo)){
		cat('',fill=TRUE)
		crea.cat.fnc('lee.archivo.fnc')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(" lee.archivo.fnc(archivo ='mis.datos.sav')				",fill=TRUE)
		cat(" lee.archivo.fnc('mis.datos.sav')						",fill=TRUE)
		cat(" lee.archivo.fnc('mis.datos.xlx') #Solo Excel 97-2003		",fill=TRUE)
		cat(" lee.archivo.fnc('mis.datos.Rdata')					",fill=TRUE)
		cat(" lee.archivo.fnc('clipboard', hay.nombres=T, delimitador='tab')",fill=TRUE)
		cat(" lee.archivo.fnc('mis.datos.txt', hay.nombres=T, dec=',',	",fill=TRUE)
		cat(" 		separador='tab', miss=c('.',-999))					",fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Lee archivos externos en formatos sav (SPSS), xls (Excel 97-2003),	', fill=TRUE)
		cat(' binarios de R (Rdata) y txt (texto plano).					', fill=TRUE)
		cat(" Cuando utilizamos como argumento de archivo 'clipboard', indicamos	", fill=TRUE)
		cat(' que deseamos incorporar a R una matriz de datos copiada en el portapapeles', fill=TRUE)
		cat(' desde otra aplicacion (excel, etc)							', fill=TRUE)
		cat(" ",fill=TRUE)
		cat(' sites.google.com/site/ullrtoolbox/lectura-de-archivos/lee-archivo-fnc', fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		print(argumentos('lee.archivo.fnc'))
	return('help')
	}

	# SI SE LEE UN BINARIO RDATA
	hay.Rdata=match('Rdata',strsplit(archivo,'[:.:]')[[1]])
	hay.excel1=match('xls',strsplit(archivo,'[:.:]')[[1]])
	hay.excel2=match('xlsx',strsplit(archivo,'[:.:]')[[1]])

	if(!is.na(hay.excel2)){
		cat('',fill=TRUE)
		cat('*** Error. El archivo excel introducido tiene formato:     ***',fill=TRUE)
		cat('*** excel 2007-2010. Debes guardarlo en excel con formato: ***',fill=TRUE)
		cat('***         Libro de excel 97-2003 (extension xls)         ***',fill=TRUE)
		stop( )
	}

	hay.spss=match('sav',strsplit(archivo,'[:.:]')[[1]])
	if(!is.na(hay.spss)) spss=TRUE
	if(!is.na(hay.excel1[1]) | !is.na(hay.excel2[1])) excel=TRUE
	if(!is.na(hay.Rdata[1])) {
		que.hay=ls( );	load(archivo);	que.hay.2=ls()
		que.hay.2=que.hay.2[-(match('que.hay',que.hay.2))]
		indice=que.hay.2 %in% que.hay
		new.var=que.hay.2[!indice]
		if(length(new.var)==0){
		  if(is.na(silente)){
		  	cat(' ',fill=TRUE)
		  	cat('*** Se ha leido correctamente el archivo externo',archivo,fill=TRUE)
		  	cat('*** La variable leida desde el archivo ya existia en el ambiente. Debes  ***',fill=TRUE)
		  	cat('*** conocer el nombre del objeto leido para poder utilizar los comandos  ***',fill=TRUE)
		  	cat('*** head(objeto) o dim(objeto) y conocer el contenido del mismo.         ***',fill=TRUE)
		  }
		}else{
		  if(is.na(silente)){
		  	cat(' ',fill=TRUE)
		  	cat('*** Se ha leido correctamente el archivo externo',archivo,'    ***',fill=TRUE)
		  	cat('*** El nuevo objeto leido es:',new.var,'.Esta es la cabecera: ***',fill=TRUE)
		  	dimension(eval(parse(text=new.var)))
		  	cat(' ',fill=TRUE)
			if(class(eval(parse(text=new.var)))=='data.frame')
			    print( head(eval(parse(text=new.var))) )
		  	if(class(eval(parse(text=new.var)))=='list')
			    print( names(eval(parse(text=new.var))) )
		  }
		return( eval(parse(text=new.var)) )
		}
	} # FIN SI SE LEE UN BINARIO RDATA

	# SI LEE BINARIO DE EXCEL
	if(excel) {
	    hoja=que.hoja
	    require(gdata,quietly =TRUE)

	    datos=try(read.xls(archivo,sheet=hoja, encoding='latin1'),silent=TRUE)
	    if(class(datos)=='try-error')
	      datos=try(read.xls(archivo,sheet=hoja, fileEncoding='latin1'),silent=TRUE)
	    if(class(datos)=='try-error') {
 	    	chivato=dir(pattern=archivo)
          	if(length(chivato)!=1){
			cat('',fill=TRUE)
			cat('***  Error. No se encuenta el archivo', archivo,'en el directorio actual ***',fill=TRUE)
			stop( )
		}else{
			cat('***  Error. Parece que no tienes instalado un interprete Perl          ',fill=TRUE)
			cat('***  la lectura de archivos de excel lo exige. Instalalo desde         ',fill=TRUE)
			cat('***  el siguiente link:                                                ',fill=TRUE)
			cat('***         http://www.activestate.com/activeperl/downloads	        ',fill=TRUE)
			cat('***  La instalacion puede tardar algunos minutos. Una vez finalizada,  ',fill=TRUE)
			cat('***  REINICIA el ordenador y ya podras leer archivos excel 2003 (xls)  ',fill=TRUE)
		stop( )
 	    	}
	    }

	   	n.suj=dim(datos)[1]; n.var=dim(datos)[2]
	   		if(is.na(silente)){
	    		cat('',fill=TRUE)
	    		cat('*** El archivo excel',archivo,'contiene',n.suj,'registros y',n.var,'variables.    ***',fill=TRUE)
	    	}
	    if(!is.na(que.hoja[1])){
	    	if(is.na(silente)){
	    		cat('*** Se ha asumido la lectura de la hoja:',hoja,'del archivo', archivo,'. Si deseas leer   ***',fill=TRUE)
	    		cat('*** otra hoja de ese archivo, incluye el argumento que.hoja (Ej. que.hoja=2) ***',fill=TRUE)
				cat(' ',fill=TRUE)
				print(head(datos))
			}
		}
	check=dime.si.hay.factores.fnc(datos)
	check$columnas
	try(detach(package:gdata),silent=TRUE)
	if(length(check$columnas)!=0)
	  for(i in check$columnas) datos[,i]=recode(datos[,i], " ''=NA ")
        return(datos)
	} # FIN BINARIO EXCEL

	if(is.na(hay.nombres)) hay.nombres=FALSE
 	if(is.na(decimal)) decimal='.'
	if(is.na(miss[1])) miss='NA'
	if(is.na(separador)) separador=''
	if(separador =='tab') separador='\t'
	if(archivo=='clipboard') separador='\t'

	if(is.na(spss[1])) {
	  datos=read.table(archivo,h=hay.nombres,dec=decimal, encoding='latin1',skip=salta,
			 nrows=nfilas, sep=separador,na.strings=miss, fill=TRUE)
	}else{
	  require(foreign, quietly =TRUE)
          ext=strsplit(archivo,'[:.:]')[[1]][2]
	  if(ext!='sav') {cat('*** El archivo binario de spss no tiene extension sav ***',fill=TRUE)
	    stop( ) }
	    if(etiquetas){
	      cat('',fill=TRUE);
	      cat('*** Si introduces el argumento: etiquetas=FALSE no se activaran las etiquetas',fill=TRUE)
	      cat('*** de los valores de las variables sino sus relativos numericos',fill=TRUE)
	      cat('',fill=TRUE);
	    }
	  datos=suppressWarnings(read.spss(archivo, to.data.frame =TRUE,
		use.value.labels =etiquetas))
		try(detach(package:foreign),silent=TRUE)
	}
	if(!is.na(traspuesta)) datos=data.frame(t(datos))
		n.suj=dim(datos)[1]; n.var=dim(datos)[2]
		if(is.na(silente)){
			cat('',fill=TRUE)
			cat('*** El archivo',archivo,'contiene',n.suj,'registros y',n.var,'variables ***',fill=TRUE)
			cat('*** Si la estructura original es de variables x sujetos, incluye en   ***',fill=TRUE)
			cat('*** la funcion el argumento traspuesta=T.                             ***',fill=TRUE)
			cat(' ',fill=TRUE)
			print(head(datos))
		}
 return(datos)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# lee.archivos.por.sujetos.fnc(que.col=c(3,10,11,13), tipo='xlsx')
# Lee archivos externos de n sujetos con las variables apiladas y las
# monta en un archivo unico apilado.
#---------------------------------------------------------------------------
 lee.archivos.por.sujetos.fnc=function(que.col, tipo='xls', separador=' ',
		decimal='.', hay.nombres=FALSE){
 	archivos=dir(pattern=tipo)
 	n.archivos=length(archivos)
	if(tipo=='xls'){
		suj=lee.archivo.fnc(archivos[1],silente=TRUE)
	}
	if(tipo=='txt'){
 		suj=lee.archivo.fnc(archivos[1],separador=separador,
			decimal=decimal, hay.nombres=hay.nombres, silente=TRUE)
	}
 	nombres=names(suj); nombres=nombres[que.col]
	n.filas=dim(suj)[1]
 	lista=list( )
 	for(i in 1:length(que.col)) lista[[i]]=data.frame(matrix(NA,n.archivos,n.filas))
 	names(lista)=nombres
 	for(i in 1:n.archivos){
 		cat('*** Leyendo archivo',archivos[i], '***',fill=TRUE)
		if(tipo=='xls'){
			suj=lee.archivo.fnc(archivos[i],silente=TRUE)
		}
		if(tipo=='txt'){
 			suj=lee.archivo.fnc(archivos[i],separador=separador,
				decimal=decimal, hay.nombres=hay.nombres, silente=TRUE)
		}
		for(j in 1:length(lista)) lista[[j]][i,]=suj[,que.col[j]]
 	}

	lista2=lapply(lista, function(x) { x=stack(x); x=data.frame(x[,1])})
	salida=do.call(cbind,lista2); names(salida)=nombres
	salida$sujeto=factor(paste('suj',1:n.archivos, sep=''),
			levels=paste('suj',1:n.archivos, sep=''))
	cat('',fill=TRUE)
	cat('*** Esta es la cabecera de los datos leidos ***',fill=TRUE)
	cat('',fill=TRUE)
	print(head(salida))
 return(salida)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# guarda.Rdata.fnc(turismo, nombre='prueba')
#---------------------------------------------------------------------------
 guarda.Rdata.fnc=function(datos=NA, nombre){
	if(class(try(is.na(datos)))=='logical') {
	    if(class(datos)!='list'){
		cat('',fill=TRUE)
		crea.cat.fnc('guarda.Rdata.fnc')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat("      guarda.Rdata.fnc(mis.datos, nombre='datos')					  ",fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Guarda en un archivo binario de R (Rdata) de nombre datos.Rdata el ', fill=TRUE)
		cat(' objeto mis.datos	                                                 ', fill=TRUE)
		cat('',fill=TRUE)
		cat(' sites.google.com/site/ullrtoolbox/lectura-de-archivos/guarda-rdata-fnc',fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
	return('help')
	   }
	}
	if(!is.na(nombre)){
	  partes=strsplit(nombre,'[:.:]')[[1]]
	  hay.Rd='Rdata' %in% partes
	  if(!hay.Rd) nombre=paste(nombre,'.Rdata',sep='')
	}else{
	  cat('',fill=TRUE)
	  cat('*** WARNING. No has dado un nombre al archivo de salida con el argumento nombre',fill=TRUE)
	  cat('*** Se utilizara como nombre del archivo el siguiente. miobjeto.Rdata. Si deseas',fill=TRUE)
	  cat("*** otro nombre utiliza el argumento nombre. Ej: guarda.Rdata.fnc(datos,'expe')",fill=TRUE)
	  cat('',fill=TRUE) 
	  name='miobjeto.Rdata'
	}
	tipo=try(save(datos, file=nombre),silent=TRUE)
	if(class(tipo)!='try-error'){
		cat('',fill=TRUE)
 		cat('*** Se ha guardado con exito el archivo con el nombre:  ',nombre,fill=TRUE)
		cat('*** para leerlo en futuras sesiones utiliza la funcion: lee.archivo.fnc' ,fill=TRUE)
		cat('*** con',nombre,'como unico argumento (entre comillas o apostrofe)',fill=TRUE)
		cat("*** Ej. lee.archivo.fnc('mi_archivo') ",fill=TRUE)
		cat('',fill=TRUE)
	}else{
		cat('',fill=TRUE)
		cat('*** Error. No ha sido posible guardar el archivo Rdata ***',fill=TRUE)
		cat('',fill=TRUE)
		stop(tipo)
	}
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# Pone como nombre de variable la secuencia de items
#---------------------------------------------------------------------------
 crea.nombre.item.fnc=function(datos=NA,col.empieza.item=NA,n.item=NA) {
	if(class(try(is.na(datos)))=='logical') {
		cat('',fill=TRUE)
		crea.cat.fnc('crea.nombre.item.fnc')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' datos= crea.nombre.item.fnc(datos)'                                  ,fill=TRUE)
		cat(' datos= crea.nombre.item.fnc(datos, col.empieza.item=6, n.item=40)'   ,fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' 20 Items que se repiten a traves de los niveles de un factor MR(2 niveles)',fill=TRUE)
		cat(' datos= crea.nombre.item.fnc(datos, col.empieza.item=6,'              ,fill=TRUE)
		cat('            n.item=c(1:10,1:10,11:20,11:20) )'                        ,fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Cuando tienes items en tu base de datos es obligatorio que cada uno', fill=TRUE)
		cat(' de ellos se nombren correlativamente con el nombre item1,item2,...itemn.', fill=TRUE)
		cat(' crea.nombre.item.fnc asigna automaticamente dicho nombre a cada columna', fill=TRUE)
		cat(' de la base de datos. Si solo hay item y estos no se repiten no se ', fill=TRUE)
		cat(' necesita incluir argumento alguno a la funcion. Si hubiera otras variables', fill=TRUE)
		cat(' con col.empieza.item indicariamos la columna de comienzo de dichos    ', fill=TRUE)
		cat(' items y el numero de estos. Si los items se repitiesen el usuario debera', fill=TRUE)
		cat(' indicarlo adecuadamente (ver ejemplo anterior)', fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
	return('help')
	}

	dat=datos
	if(is.na(n.item[1])) n.item=dim(dat)[2]
	if(is.na(col.empieza.item[1])) col.empieza.item=1
	if(length(n.item)==1){
		nombres=names(dat)
		nombres[col.empieza.item:(col.empieza.item+n.item-1)]=
			paste('item',1:n.item,sep='')
		names(dat)=nombres
	}else{
		nombres=names(dat)
		n.item_=length(n.item)
		nom=paste('item',n.item,sep='')
		nombres[col.empieza.item:(n.item_+(col.empieza.item-1))]=nom
		names(dat)=nombres
	}
 return(dat)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#  contraste.t.intergrupo.fnc(datos,vd='pre.2',vi='gender')
#---------------------------------------------------------------------------
 contraste.t.intergrupo.fnc=function(datos=NA,vd,vi, identifica=FALSE, silente=FALSE, latex=FALSE){
	if(class(try(is.na(datos)))=='logical') {
		cat('',fill=TRUE)
		crea.cat.fnc('contraste.t.intergrupo.fnc')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(" contraste.t.intergrupo.fnc(OBrienKaiser, vd='pre.1', vi='gender')   ",fill=TRUE)
		cat(" contraste.t.intergrupo.fnc(OBrienKaiser, vd='pre.1', vi='gender',   ",fill=TRUE)
		cat(" 		identifica=T)                                                 ",fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Contraste t de una variable dependiente sobre los dos niveles del    ', fill=TRUE)
		cat(' factor incluido en el argumento vi. El usuario puede incluir el      ', fill=TRUE)
		cat(' argumento identifica=T si desea detectar casos atipicos o extremos   ', fill=TRUE)
		cat(' desde el diagrama de cajas que se genera automaticamente.            ', fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
	return('help')
	}
	
	if(length(vd) > 1){
	    ret=multiple.ttest.fnc(datos,vd=vd,vi=vi)
	return(ret)
	}
	
	require(car, quietly =TRUE)
	nombres=names(datos)
	if(is.numeric(vd[1])){
		vd=nombres[vd]
	}
	if(is.numeric(vi[1])){
		vi=nombres[vi]
	}

	# EXISTEN LAS VARIABLES?
	check=existe.variable.fnc(datos,c(vd,vi))
	if(check$cc !=0) {
		cat('',fill=TRUE)
		cat('*** Error. No existe la variable o variables:',check$faltan,fill=TRUE)
		cat('*** en la base de datos incluida. Revisa los argumentos',fill=TRUE)
		stop( )
	}

	if(is.numeric(vi[1])){
		vi=nombres[vi]
	}
	datos[,vi]=as.factor(as.character(datos[,vi]))
	if(nlevels(datos[,vi]) > 2) {
		cat('',fill=TRUE)
		cat('*** Error. El numero maximo de niveles de vi es 2 ***',fill=TRUE)
	 stop( )
	}

	if(vd==vi) stop('La variable dependiente no puede ser igual a la independiente')

	medias=tapply(datos[,vd],datos[,vi],function(x) mean(x,na.rm=TRUE))
	dt=tapply(datos[,vd],datos[,vi],function(x) sd(x,na.rm=TRUE))
	n=round(tapply(datos[,vd],datos[,vi],length),0)

	levene=leveneTest(datos[,vd],datos[,vi])
	tabla=rbind(medias,dt,n)
	limites=c(0, max(medias) + mean(dt))
	require(lattice, quietly=TRUE)
	if(identifica){
	  diagrama.cajas.fnc(datos, vd=vd, que.factor=vi, identifica=identifica)
	}else{
	  print(bwplot(datos[,vd]~datos[,vi],xlab=vi,ylab=vd,main='Diagrama de cajas'))
        }

	# HOMOGENEIDAD
	contraste=t.test(datos[,vd] ~ datos[,vi],paired=FALSE,var.equal=TRUE)
	t.ho=as.numeric(contraste$statistic)
	gr.lib.ho=as.numeric(contraste$parameter)
	p.ho=as.numeric(contraste$p.value)
	dif.ho=as.numeric(sum(medias*c(1,-1)))
	et.dif.ho=dif.ho/t.ho
	ic.ho=as.numeric(contraste$conf.int)
	varianzas.homogeneas=c(t=t.ho,gl=gr.lib.ho,p=p.ho,err.t.ho=et.dif.ho,
		diferencia=dif.ho,ic.95.=ic.ho)
	varianzas.homogeneas= round(varianzas.homogeneas,5)

	# HETEROGENEIDAD
	contraste=t.test(datos[,vd] ~ datos[,vi],paired=FALSE,var.equal=FALSE)
	t=as.numeric(contraste$statistic)
	gr.lib=as.numeric(contraste$parameter)
	p=as.numeric(contraste$p.value)
	et.dif=as.numeric(sum(medias*c(1,-1))/t)
	ic=as.numeric(contraste$conf.int)
	varianzas.heterogeneas=c(t=t,gl=gr.lib,p=p,err.t=et.dif,diferencia=dif.ho,ic.95.=ic)
	varianzas.heterogeneas= round(varianzas.heterogeneas,5)

	resultado=rbind(varianzas.homogeneas,varianzas.heterogeneas)

	# effects size HETEROG.
	delta= abs(t)*sqrt( (1/as.numeric(n[1]))+as.numeric((1/n[2])))
	r2=abs(t)^2/(abs(t)^2+gr.lib)
	n.a=as.numeric((2*n[1]*n[2])/(n[1]+n[2]))
	no.centralidad=delta*sqrt(n.a/2)
	potencia.observada=power.t.test(delta=delta, n=n.a, sd=1,sig.level=0.05)$power
	size=c(delta=delta,r2=r2,par.no.centralidad=no.centralidad,
			pot.obser.=potencia.observada)
	size=round(size,4)

	# effects size HOMOGENEIDAD.
	delta.ho= abs(t.ho)*sqrt( (1/as.numeric(n[1]))+as.numeric((1/n[2])))
	no.centralidad.ho=delta.ho*sqrt(n.a/2)

	r2.ho=abs(t.ho)^2/(abs(t.ho)^2+gr.lib.ho)
	n.a=as.numeric((2*n[1]*n[2])/(n[1]+n[2]))
	potencia.observada.ho=power.t.test(delta=delta.ho, n=n.a, sd=1,sig.level=0.05)$power
	size.ho=c(delta=delta.ho,r2=r2.ho,par.no.centralidad=no.centralidad.ho,
		pot.obser.=potencia.observada.ho)
	size.ho=round(size.ho,4)

	efectos=rbind(varianzas.homogeneas=size.ho,varianzas.heterogeneas=size)
	lista=list(paste('contraste.t VD:',vd,'por VI:',vi,sep=' '),descriptivos=tabla,
			test.levene=levene,contraste=resultado,efectos=efectos)
	if(!silente) crea.cat.fnc('PRUEBA T PARA GRUPOS INDEPENDIENTES')
 if(latex) latex.fnc(lista)	
# try(detach(package:car), silent=TRUE)
 return(lista)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
 multiple.ttest.fnc=function(datos, vd, vi){
    nombres=names(datos)
    if(is.numeric(vd[1])) vd=nombres[vd]
    lista=list( )
    crea.cat.fnc('PRUEBA T PARA GRUPOS INDEPENDIENTES')
    for(i in vd) 
	lista[[i]]= contraste.t.intergrupo.fnc(datos,vd=i, vi=vi, silente=TRUE)
 return(lista)
}
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#   contraste.t.intragrupo.fnc(dat,par=c('pre.1','pre.2'),elimina.outliers=FALSE)
#---------------------------------------------------------------------------
 contraste.t.intragrupo.fnc=function(datos=NA,par,elimina.outliers=FALSE, latex=FALSE){
	if(class(try(is.na(datos)))=='logical') {
		cat('',fill=TRUE)
		crea.cat.fnc('contraste.t.intragrupo.fnc')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(" contraste.t.intragrupo.fnc(OBrienKaiser, par=c('pre.1','pre.2') )   ",fill=TRUE)
		cat(" contraste.t.intragrupo.fnc(OBrienKaiser, par=3:4, elimina.outlier=T)",fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Contraste t de medidas repetidas. El usuario introduce en el         ', fill=TRUE)
		cat(' argumento par las dos variables cuantitativas que constituyen las   ', fill=TRUE)
		cat(' medidas que se contrastan. Si se utiliza el argumento elimina.outlier=T', fill=TRUE)
		cat(' se eliminaran aquellos registros cuya diferencia entre las medidas  ', fill=TRUE)
		cat(' vayan en sentido contrario a la mayoria de las diferencias.         ', fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
	return('help')
	}
	dat=datos
	if(is.numeric(par)){
		nombres=names(dat)
		par=nombres[par]
	}

	# EXISTEN LAS VARIABLES
	check=existe.variable.fnc(datos,par)
	if(check$cc !=0) {
		cat('',fill=TRUE)
		cat('*** Error. No existe la variable o variables:',check$faltan,fill=TRUE)
		cat('*** en la base de datos incluida. Revisa el argumento par',fill=TRUE)
		stop( )
	}

 	dat=dat[,par]
 	dat=na.omit(dat)

	crea.cat.fnc('PRUEBA T DE PARES RELACIONADOS')

	# Correlaciones de las puntuaciones
	cont.cor=cor.test(dat[,1],dat[,2])
	cor.c=cont.cor$estimate
	t.c=cont.cor$statistic
	gl.c= cont.cor$parameter
	p.c= round(cont.cor$p.value,6)
	correlacion=c(cor.c,t.c,gl.c,p=p.c)

 	n.suj=dim(dat)[1]
	datos.inter=apila.los.datos.fnc(dat,col.empieza.mr=1,
		fac.intra=list(factor_=names(dat)),fac.inter=NA, silente=TRUE)
	niveles=levels(datos.inter$factor_)
	Ftrat=as.numeric(datos.inter$factor_)
	medias=with(datos.inter,tapply(vd,factor_,function(x) mean(x,na.rm=TRUE))); medias
	mean.dif=sum(medias*c(1,-1))
	dif=dat[,1]-dat[,2]
	incongruente=dif

	if(mean.dif > 0) {
		incong= with(dat, dat[,2] > dat[,1])
	}else{
		incong= with(dat, dat[,2] < dat[,1])
	}
	dat.nc=dat[incong,]
	dat.c=dat[!incong,]
	if(dim(dat.nc)[1]!=0){
 		row.names(dat.nc)= paste('sujeto',row.names(dat.nc))
		incongruentes=list(sujetos.incongruentes=dat.nc)
	}else{
		incongruentes=list(sujetos.incongruentes='No hay sujetos incongruentes')
	}
	attach(datos.inter)
	par(mfrow=c(1,2))
		plot(c(0.5,2.5), range(datos.inter$vd),
			type='n', axes=FALSE, xlab='factor', ylab='vd',
			main='Consistencia entre-sujetos')
 			axis(1,at=1:2,labels=niveles);
 			axis(2);
 			box()
 		points(Ftrat[factor_==niveles[1]],
 			datos.inter[datos.inter$factor_==niveles[1],'vd'],pch=19,cex=0.5)
 		points(Ftrat[factor_==niveles[2]],
 			datos.inter[datos.inter$factor_==niveles[2],'vd'],pch=19,cex=0.5)
 		for (i in 1:dim(dat.c)[1])
 				{lines(1:2, dat.c[i,],lty=1,lwd=1,type='l',
				pch=1,cex=1,col='blue') }
 		for (i in 1:dim(dat.nc)[1])
 				{lines(1:2,dat.nc[i,],lty=1,lwd=2,type='l',
				pch=1,cex=1,col='red')}
 	boxplot(dif,main='Diagrama de cajas de las diferencias')
	par(mfrow=c(1,1))
 	try(detach(datos.inter),silent=TRUE)

 	contraste=t.test(dat[,1],dat[,2],paired=TRUE)
 	names(contraste)
 	t=as.numeric(contraste$statistic)
 	gr.lib=as.numeric(contraste$parameter)
 	p=as.numeric(contraste$p.value)
 	et.dif=abs(mean.dif/t)
 	ic=as.numeric(contraste$conf.int)
 	resultado=c(t=t,gl=gr.lib,p  =p,err.t=et.dif,diferencia=mean.dif,ic.95.=ic)
 	resultado=round(resultado,3)
	# effects size
	sigma=(mean.dif/t)*sqrt(n.suj)
	delta= abs(mean.dif)/sigma
	r2=(delta/(sqrt(delta^2+ 1/0.25)))^2
	potencia.observada=power.t.test(delta=delta, n=n.suj,
			sd=1,sig.level=0.05,type='paired')$power
	size=c(delta=delta,r2=r2,pot.obser.=potencia.observada)
	size=round(size,3)

	if(elimina.outliers){
		dat=dat[!incong,]
		# Correlaciones de las puntuaciones
		cont.cor=cor.test(dat[,1],dat[,2])
		cor.c=cont.cor$estimate
		t.c=cont.cor$statistic
		gl.c= cont.cor$parameter
		p.c= round(cont.cor$p.value,6)
		correlacion.sin.outliers=c(cor.c,t.c,gl.c,p=p.c)

		n.suj.2=dim(dat)[1]

		datos.inter=apila.los.datos.fnc(dat,col.empieza.mr=1,
			fac.intra=list(factor_=names(dat)),fac.inter=NA, silente=TRUE)
		medias.2=with(datos.inter,tapply(vd,factor_,mean)); medias
		mean.dif.2=sum(medias.2*c(1,-1))
		dif.2=dat[,1]-dat[,2]
 		contraste.2=t.test(dat[,1],dat[,2],paired=TRUE)
 		t.2=as.numeric(contraste.2$statistic)
 		gr.lib.2=as.numeric(contraste.2$parameter)
 		p.2=as.numeric(contraste.2$p.value)
 		et.dif.2=abs(mean.dif.2/t.2)
 		ic.2=as.numeric(contraste.2$conf.int)
 		resultado.2=c(t=t.2,gl=gr.lib.2,p =p.2,err.t=et.dif.2,
			diferencia=mean.dif.2,ic.95.=ic.2)
 		resultado.2=round(resultado.2,3)
 		# effects size
		sigma.2=(abs(mean.dif.2)/abs(t.2))*sqrt(n.suj.2)
		delta.2= abs(mean.dif.2)/sigma.2
		r2.2=t.2^2/(t.2^2+gr.lib.2)
		potencia.observada.2=power.t.test(delta=delta.2, n=n.suj.2,
				sd=1,sig.level=0.05,type='paired')$power
		size.2=c(delta=delta.2,r2=r2.2,pot.obser.=potencia.observada.2)
		size.2=round(size.2,3)

		lista=list('contraste.t medidas repetidas'=par,
				incongruentes=incongruentes,
				rxy=correlacion,
				contraste=resultado,efectos=size,
				rxy.sin.outliers=correlacion.sin.outliers,
				contraste.sin.outliers=resultado.2, efectos.sin.outliers=size.2)
	}else{
 		lista=list('contraste.t medidas repetidas'=par,
				incongruentes=incongruentes,
				rxy=correlacion,
				contraste=resultado,efectos=size)
 	}
 if(latex){
 	lista2=lista
 	for(i in 1:length(lista2)) lista2[[i]]=as.data.frame(t(lista[[i]]))
 	print(latex.fnc(lista2[3:5]))
 	rm(lista2)
 }
 return(lista)
 }
#---------------------------------------------------------------------------
# SIMULADOR DE LA DISTRIBUCION T NO CENTRADA
#---------------------------------------------------------------------------
 require(MASS, quietly=TRUE)
 extrae.muestras.t.nocentrada.fnc=function(pobla1,pobla2,n,replica) {
	alma=matrix(0,replica,7)
    	for (i in 1:replica) {
		indice1=sort(sample(length(pobla1),n[1]))
		indice2=sort(sample(length(pobla1),n[2]))
		samp1=pobla1[indice1];	samp2=pobla2[indice2];
		dif=mean(samp1)-mean(samp2)
		t=t.test(samp1,samp2,paired=F)$statistic
		vari=( var(samp1)*(n[1]-1) + var(samp2)*(n[2]-1))/(sum(n)-2)
		delta=abs(dif)/sqrt(vari);	r.2=t^2/(t^2 +sum(n)-2)
		alma[i,1]=dif;	alma[i,2]=vari;	alma[i,3]=delta
		alma[i,4]=t;	alma[i,5]=r.2
		alma[i,6]=mean(samp1);	alma[i,7]=mean(samp2)
    }
 colnames(alma)=c('dif','var','delta','t','r.2','med1','med2')
 return(data.frame(alma))
 }

 grafica.resultados.t.nocentrada.fnc=function(result, replica,n){
	medias=colMeans(result)
	delta_=medias[3]; med1=medias[6]; med2=medias[7]
	na=2*(n[1]*n[2])/(n[1]+n[2])
	DELTA=as.numeric(delta_)*sqrt(na/2)
     if(mean(result$t) < 0) DELTA=DELTA*-1
 	x=seq(min(result$t),max(result$t),0.01)
 	x.dif=seq(min(result$dif),max(result$dif),0.01)
 	x=seq(min(result$t),max(result$t),0.01)
 	x.dif=seq(min(result$dif),max(result$dif),0.01)
     limites=c(range(result$med1),range(result$med2))
	xlim=c(min(limites),max(limites))
	xlim.p=c(min(pobla1),max(pobla2))
 	par(mfrow=c(2,3))
		plot(density(pobla1),col='blue',lwd=2,xlim=xlim.p,
			main='Poblacion1 y 2')
			lines(density(pobla2),col='red',lwd=2,xlim=xlim.p)
			abline(v=c(mean(pobla2),mean(pobla1)),col=c(2,4),lwd=2)
		plot(density(result$med1),col='blue',lwd=2,xlim=xlim,
			main='Distribucion de medias')
		lines(density(result$med2),col='red',lwd=2,xlim=xlim)
		abline(v=c(mean(result$med2),mean(result$med1)),col=c(2,4),lwd=2)
  		truehist(result$dif,main='diferencias no tipificadas')
			abline(v=mean(result$dif),col='red',lwd=2)
			lines(x.dif,dnorm(x.dif,mean(result$dif),sd(result$dif)),
				col='blue',lwd=2)
  		truehist(result$delta,main='diferencias tipificadas (delta)')
			abline(v=mean(result$delta),col='red',lwd=2)
 		truehist(result$t,main='t.empirica y teorica con DELTA')
 			lines(x, dt(x,sum(n)-2,DELTA),lwd=2,col='blue')
			abline(v=mean(result$t),col='red',lwd=2)
 		truehist(result$r.2,main='r.2')
			abline(v=quantile(result$r.2,0.5),col='red',lwd=2)
  	par(mfrow=c(1,1))
 	ic=c(quantile(result$r.2,c(0.025,0.5, 0.975)))
	Deltas=c(DELTA.Esperada=DELTA, MEDIA.T=mean(result$t))
	Potencia=sum(abs(result$t) >= abs(qt(0.975,sum(n)-2)))/replica
	lista=list(Deltas=Deltas,Potencia.Empirica=Potencia, IC.r2=round(ic,4))
 	return(lista)		
 }

 simula.t.nocentrada.fcn=function(pobla1,pobla2,n,replica) {
	resultados=extrae.muestras.t.nocentrada.fnc(pobla1,pobla2,n,replica)
 	grafica.resultados.t.nocentrada.fnc(resultados,replica,n)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# analisis.factorial.fnc(dat, mat.cor=NA, tipo=NA,variables=NA,rotacion='varimax',
#						n.factores=NA, save.pf=FALSE, corte=0.25 )
# tipo=c('cp','af','ml')
# cp= componentes principales
# af= analisis factorial ejes principales minres
#---------------------------------------------------------------------------
 analisis.factorial.fnc=function(datos=NA, matcor=NA, variables=NA, tipo=NA, rotacion=NA, 
		n.factores=NA, grafica=FALSE, guarda.pf=FALSE, caso.completo=FALSE, simula=FALSE,
		n=200, parallel=FALSE, policorica=FALSE, tetracorica=FALSE, corte=0.25, size.font=1,
		return.res=FALSE, latex=FALSE){

	if(class(try(is.na(datos)))=='logical' & is.na(matcor[1])[1]) {
		cat('',fill=TRUE)
		crea.cat.fnc('analisis.factorial.fnc')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat("  analisis.factorial.fnc(iqitems)                                      ",fill=TRUE)
		cat("  analisis.factorial.fnc(iqitems, variables=1:14, n.factores=3)       ",fill=TRUE)
		cat('', fill=TRUE)
		cat("  correla=correlacion.fnc(iqitems, tipo='policorica', silente=T)       ",fill=TRUE)
		cat("  analisis.factorial.fnc(iqitems, matcor='correla', rotacion='oblimin',",fill=TRUE)
		cat("  		n.factores=3 tipo='af', grafica=T)                              ",fill=TRUE)
		cat('', fill=TRUE)
		cat("  datos= analisis.factorial.fnc(iqitems, variables=1:14,               ",fill=TRUE)
		cat("  		rotacion='oblimin', n.factores=3, tipo='af',                    ",fill=TRUE)
		cat("  		grafica=T, guarda.pf=T)                                         ",fill=TRUE)		
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Realiza analisis factorial y de componentes principales con o sin	  ', fill=TRUE)
		cat(' rotacion. Esta puede ser ortogonal: varimax o quartimax             ', fill=TRUE)
		cat(' u oblicua: oblimin, promax o simplimax.                             ', fill=TRUE)
		cat(' Pueden guardarse las puntuaciones factoriales por sujeto o registro ', fill=TRUE)
		cat(' incluyendo el argumento guarda.pf=T y asignando la salida.          ', fill=TRUE)
		cat('', fill=TRUE)
		cat(" ",fill=TRUE)
		cat("             https://sites.google.com/site/ullrtoolbox               ", fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		print(argumentos('analisis.factorial.fnc'))
	return('help')
	}
	
	require(psych, quietly=TRUE)
	hay.suj=TRUE
	# SI NA DATOS Y !NA MATCOR
	if(class(try(is.na(datos)))=='logical' & !is.na(matcor)[1]){
		hay.suj=FALSE; triangular=FALSE
		nomb=colnames(matcor);	nvar=dim(matcor)[2]
		if(is.null(nomb)) nomb=paste('V',1:nvar,sep='')
		if(is.data.frame(matcor)) matcor=as.matrix(matcor)
		
		if(is.na(matcor[,2])[1]) {
			matcor=na.omit(matrix(t(matcor)))
			triangular=TRUE
		}
		if(is.na(matcor[,1])[2]){
			matcor=na.omit(matrix(t(matcor)))
			triangular=TRUE
		}
		if(triangular){
			require(lavaan, quietly=TRUE)
			matcor=getCov(matcor,names=nomb)
		}
		# FIN SI TRIANGULAR
		cor.dat=try(cor.smooth(matcor),silent=T)
		datos=data.frame(mvrnorm(n, rep(0,dim(matcor)[2]), Sigma=cor.dat))
		if(guarda.pf & !simula){
			cat('',fill=TRUE)
			cat('*** WARNING. No pueden generarse puntuaciones factoriales dado que no has   ***',fill=TRUE)
			cat('*** introducido ninguna base de datos. El modelo se estima desde la matriz  ***',fill=TRUE)
			cat('*** de correlaciones.                                                       ***',fill=TRUE)
			cat('*** Si deseas generar una muestra aleatoria para la matriz de correlaciones ***',fill=TRUE)
			cat('*** incluida y poder asi estimar las puntuacions factoriales incluye el     ***',fill=TRUE)
			cat('*** argumento simula=T, si omites el argumento n este valdra n=200          ***',fill=TRUE)
			cat('',fill=TRUE)
			con='d'
			while(con!='c') {
				con=readline('***********              PULSA C PARA CONTINUAR             ***********') 
				if(con=='C') con='c'
			} 
			guarda.pf=FALSE
		}
 		if(!guarda.pf & simula) {guarda.pf=TRUE; hay.suj=TRUE}
 		if(guarda.pf & simula) hay.suj=TRUE
		try(detach(package:lavaan), silent=TRUE)
	}	
	# FIN 

	row.names(datos)=1:dim(datos)[1]
	dat=datos
	datos$sujeto=row.names(datos)
	require(GPArotation, quietly = TRUE)

	crea.cat.fnc('ANALISIS FACTORIAL')

	# CHECK SI HAS ASIGNADO OBJETO DE SALIDA
	if(guarda.pf){
		cat('',fill=TRUE)
		cat('*** Has solicitado guardar las puntuaciones factoriales por sujeto.    ***',fill=TRUE)
		cat('*** Para que se guarden de hecho asegurate de haber ASIGNADO de forma  ***',fill=TRUE)
		cat('*** adecuada la salida de esta funcion a un objeto cualquiera.         ***',fill=TRUE)
		cat("*** Ej. datos=analisis.factorial.fnc(datos, n.factores=3, guarda.pf=T) ***",fill=TRUE)
		cat('',fill=TRUE)
		con='d'
		while(con!='c') {
			con=readline('*********** PULSA C PARA CONTINUAR o A para ABORTAR  ***********') 
			if(con=='C') con='c'
			if(con=='A' | con=='a') { 
				con='c'; abortar=TRUE
			}else{
				abortar=FALSE
			} 
		}
		if(abortar){
			cat('',fill=TRUE)
			cat('*** Asigna la salida al objeto que desees y repite la llamada ***',fill=TRUE)
			return( )
		}
	}
	# FIN CHECK SI HAS ASIGNADO OBJETO DE SALIDA

	# CHECK SI SE HA INTRODUCIDO UNA MATRIZ DE CORRELACIONES
	hay.mat=FALSE
	if(!is.na(matcor)[1]) hay.mat=TRUE
	if(!is.na(matcor)[1] & hay.suj){
		hay.mat=TRUE
		nombres=names(data.frame(matcor))
		indice=nombres %in% names(dat)
		if(sum(!indice) !=0){
			cat('',fill=TRUE)
			cat('*** Error, no existe la variable:',nombres[!indice],'de la matriz de correlaciones',fill=TRUE)
			cat('*** en la base de datos que has introducido en la funcion',fill=TRUE)
			cat('*** Revisa tus datos.',fill=TRUE)
			cat('',fill=TRUE)
			stop( )
		}else{
			variables=nombres
		}
	}
	# FIN CHECK SI SE HA INTRODUCIDO UNA MATRIZ DE CORRELACIONES

	if(is.na(variables[1])) {
		variables=names(dat)
	}else{
		dat=dat[,variables]
	}
	
	hay.factores=dime.si.hay.factores.fnc(dat)
	nvar=length(variables)
	nfac=length(hay.factores[[1]])
	if(nfac > 0){
		if(nfac != nvar){
		cat('',fill=TRUE)
		cat('*** Error. Hay factores entre las variables implicadas. No puede calcularse la  ***',fill=TRUE)
		cat('*** matriz de covarianzas si todas las variables a analizar no son continuas.   ***',fill=TRUE)
		cat('*** Estas son tus variables no cuantitativas erroneamente incluidas:            ***',fill=TRUE)
		cat('',fill=TRUE)
		cat('*** factores: ',hay.factores[[2]],fill=TRUE)
		cat('*** Elimina estas columnas del argumento variables: ',hay.factores[[1]],fill=TRUE)
		stop( )
		}else{
		cat('',fill=TRUE)
		cat('*** Error. Todas las variables son factores. Parecen importadas desde SPSS, ***',fill=TRUE)
		cat('*** desde donde incorrectamente se definieron como Cadena. O quizas desde   ***',fill=TRUE)
		cat('*** EXCEL donde las celdillas estan definidas como texto no como numerica.  ***',fill=TRUE)
		cat('*** Puedes resolverlo dentro de esas aplicaciones:                          ***',fill=TRUE)
		cat('*** SPSS: Elimina las etiquetas de valor de las variables o item a analizar ***',fill=TRUE)
		cat('*** EXCEL: Raton derecho -> Formato de celda. Selecciona numerica           ***',fill=TRUE)
		cat('*** En R puedes aprender a transformalas o recodificarlas si sigues estos links',fill=TRUE)
		cat('',fill=TRUE)
		cat(' https://sites.google.com/site/ullrtoolbox/02-manipulacion-de-datos/transforma-variable-fnc ',fill=TRUE)
		cat(' https://sites.google.com/site/ullrtoolbox/02-manipulacion-de-datos/recodificar ',fill=TRUE)
		cat('',fill=TRUE)
		stop( )
		}
	}

	# CHECK SI ORDINAL O DICOTOMICA
	n=dim(dat)[1]; nvar=dim(dat)[2]
	muestra=extrae.muestra.fnc(dat,n=ceiling(5*n/100),silent=T)	
	muestra_=muestra
	alma=integer( )
	for(i in 1:nvar) {
		alma=c(alma,length(unique(muestra[,i])))
		muestra_[,i]=trunc(muestra_[,i])
	}
	es.entero=sum(muestra-muestra_,na.rm=T)==0
	chivato=c(min(alma),max(alma))
	if(chivato[2] <=8 & chivato[2] > 2 & es.entero) es.ordinal=TRUE else es.ordinal=FALSE
	if(chivato[2] <=2  & es.entero) es.dicotomica=TRUE else es.dicotomica=FALSE
	# FIN SI ORDINAL O DICOTOMICA
	
	# CHECK DE MISSING
 	nas=sum(is.na(dat))
	n.suj.na=dim(dat[apply(is.na(dat),1,sum) > 0,])[1]
	var.na=variables[apply(is.na(dat),2,sum) > 0 ]
	n.var.na=length(var.na)
	que.mis=c(n.datos.NA=nas, n.var.NA=n.var.na,n.sujetos.NA=n.suj.na)

	if(nas > 0 & !caso.completo){
		cat('',fill=TRUE)
		cat('*** Tu base de datos, tiene',nas,'valores perdidos en',n.suj.na,'sujetos.',fill=TRUE)
		cat('*** Por defecto se estimara la matriz pairwise (diferente numero de sujetos  ',fill=TRUE)
		cat('*** por correlacion). Si deseas que los casos perdidos se eliminen por sujeto ',fill=TRUE)
		cat('*** (un valor perdido en cualquier variable elimina al sujeto completo),     ',fill=TRUE)
		cat('*** introduce el argumento caso.completo=T.                                        ',fill=TRUE)
		cat('',fill=TRUE)
	}

	if(caso.completo) {
		if(hay.mat){
			cor.dat=matcor
		}else{
		if(policorica){
			cat('---------------------------------------------------------------',fill=TRUE)
			cat('*** La estimacion de la matriz de correlaciones policorica es  ',fill=TRUE)
			cat('*** computacionalmente muy intensiva. Se paciente. Haz clic    ',fill=TRUE)
			cat('*** en consola si deseas ver la evolucion del proceso. ',fill=TRUE)
			cat('---------------------------------------------------------------',fill=TRUE)
 			cor.dat=polychor(na.omit(dat), smooth=TRUE, global=TRUE, polycor=T,
	 				ML = FALSE, std.err=FALSE)
			cor.dat=cor.dat[[1]]
		}
		if(tetracorica){
			dicot=length(unique(dat[,1]))==2
			if(!dicot){
				cat('',fill=TRUE)
				cat('*** Error. La correlacion tetracorica es solo para variables dicotomicas  ***',fill=TRUE)
				cat('*** elimina el argumento tetracorica=T de la llamada a la funcion.        ***',fill=TRUE)
				cat('',fill=TRUE)
			stop( )
			}
			cor.dat=tetrachoric(dat,correct=TRUE,smooth=TRUE,global=TRUE)
			cor.dat=cor.dat[[1]]
		}
		if(!policorica & !tetracorica){
			require(psych, quietly=TRUE)
			cor.dat=correlacion.fnc(dat, caso.completo=TRUE, silente=T)
			cor.dat=try(cor.smooth(cor.dat),silent=T)
			if(class(cor.dat)=='try-error'){
				cat('',fill=TRUE)
				print(cor.dat)
				stop( )
			}					
		}
		} # FIN SI HAY.MAT

		dat_=dat
		dat=na.omit(dat)
		size=dim(dat)
	}else{
		if(hay.mat){
			cor.dat=matcor
		}else{
		if(policorica){
			cat('---------------------------------------------------------------',fill=TRUE)
			cat('*** La estimacion de la matriz de correlaciones policorica es  ',fill=TRUE)
			cat('*** computacionalmente muy intensiva. Se paciente. Haz clic    ',fill=TRUE)
			cat('*** en consola si deseas ver la evolucion del proceso. ',fill=TRUE)
			cat('---------------------------------------------------------------',fill=TRUE)
 			cor.dat=polychoric(dat, smooth=TRUE, global=TRUE, polycor=T,
	 				ML = FALSE, std.err=FALSE)
			cor.dat=cor.dat[[1]]
		}
		if(tetracorica){
			dicot=length(unique(dat[,1]))==2
			if(!dicot){
				cat('',fill=TRUE)
				cat('*** Error. La correlacion tetracorica es solo para variables dicotomicas  ***',fill=TRUE)
				cat('*** elimina el argumento tetracorica=T de la llamada a la funcion.        ***',fill=TRUE)
				cat('',fill=TRUE)
			stop( )
			}
			cor.dat=tetrachoric(dat,correct=TRUE,smooth=TRUE,global=TRUE)
			cor.dat=cor.dat[[1]]
		}
		if(!policorica & !tetracorica){
			cor.dat=correlacion.fnc(dat, caso.completo=F, silente=T)
			cor.dat=try(cor.smooth(cor.dat),silent=T)
			if(class(cor.dat)=='try-error'){
				cat('',fill=TRUE)
				cat('*** Error la version de R no esta correctamente actualizada.     ***',fill=TRUE)
				cat('*** Descarga R desde http://cran.r-project.org/bin/windows/base/ ***',fill=TRUE)
				cat('*** e instalalo nuevamente como Administrador.                   ***',fill=TRUE)
				stop( )
			}
		}
		} # FIN SI HAY.MAT

		size=dim(dat)
		dat_=dat
	}

	# SUPUESTOS
	barlett=cortest.bartlett(cor.dat,n = as.numeric(size[1]))
	barlett=c(barlett[[1]],barlett[[3]],barlett[[2]])
	names(barlett)=c('chi2','gl','p.val')

	size=c(sujetos=size[1],variables=size[2])
	nvar=(dim(dat)[2])

	if(hay.suj){
		mardia_=try(mardia(dat,plot=FALSE),silent=TRUE)
		que.clase=class(mardia_)
		if(que.clase[1] !='try-error'){
     			mardia_=data.frame(matrix(c(mardia_$b1p, mardia_$skew, mardia_$p.skew,
		 		mardia_$b2p, mardia_$kurtosis, mardia_$p.kurt),
				byrow=TRUE, nrow=2))
			row.names(mardia_)=c('Asimetria','Apuntamiento')
			names(mardia_)=c('Mardia','Valor','p.val')
		}else{
			mardia_='No se han podido calcular los estadisticos de mardia'
		}
	}else{
			mardia_='No se ha incluido una matriz de datos'
	}

	#KMO y MSA
        kmo=try(paf2.fnc(cor.dat),silent=TRUE)
	if(class(kmo)[1]=='try-error') {
	   kmo=list( )
	   kmo$KMO=NA
	   kmo$MSA=NA
	}

 	if(is.na(rotacion[1])) rotacion='varimax'
 	if(is.na(tipo[1]) ) tipo='cp';
	if(rotacion != 'varimax' & rotacion != 'promax' & rotacion !='oblimin' &
		rotacion != 'quartimax' & rotacion !='simplimax') {
		cat('',fill=TRUE)
		cat("*** Error. El argumento rotacion solo puede ser:     ***",fill=TRUE)
		cat("*** ORTOGONALES:    'varimax', 'quartimax'           ***",fill=TRUE)
		cat("*** NO ORTOGONALES: 'oblimin', 'promax', 'simplimax' ***",fill=TRUE)
		cat('',fill=TRUE)
		stop( )
	}

	if(tipo !='cp' & tipo !='af') {
		cat('',fill=TRUE)
		cat("*** Error. El argumento tipo solo puede ser 'cp' o 'af'   ***",fill=TRUE)
		cat("*** COMPONENTES PRINCIPALES: 'cp'                         ***",fill=TRUE)
		cat("*** ANALISIS FACTORIAL: 'af'                              ***",fill=TRUE)
		cat('',fill=TRUE)
		stop( )
	}
	
	if(tipo=='cp') label.tipo='COMPONENTES PRINCIPALES'
	if(tipo=='af') label.tipo='ANALISIS FACTORIAL (minres)'
	if(policorica) label.tipo=paste(label.tipo,'  cor: Policorica',sep='')
	if(tetracorica) label.tipo=paste(label.tipo,'  cor: Tetracorica',sep='')
	if(!policorica & !tetracorica & is.na(matcor[1])[1]) 
		label.tipo=paste(label.tipo,'  cor: Pearson',sep='')
	if(!is.na(matcor[1])[1]) label.tipo=paste(label.tipo,'  cor: Introducida por el usuario',sep='')

	if(rotacion=='oblimin' | rotacion=='promax' | rotacion=='simplimax') rota='oblicua'
	if(rotacion=='varimax' | rotacion=='quartimax') rota='ortogonal'

	autovalores=round(eigen(cor.dat)$values,2)

	if(grafica){
		if(simula){
			grafica.correlacion.fnc(dat[,-c(nvar+1)])
		}else{
			grafica.correlacion.fnc(cor.dat)
		}
 		X11( )
		ylim=c(0,max(autovalores)+max(autovalores)*0.1)
		barplot(autovalores, col = as.numeric(autovalores >=1 ),
			ylim=ylim,
			xlab = "",
			ylab = "autovalores",
			main='Scree plot')
			abline(h = 1,lty=2,col='red'); box( )
	}

	if(is.na(n.factores[1])) {
		if(es.ordinal & !policorica & is.na(matcor)[1]){
		cat('',fill=TRUE)
		cat('------------------------------------------------------------------------------------',fill=TRUE)
		cat('*** WARNING. Tus datos parecen ordinales y tienen menos de 10 valores posibles.  ***',fill=TRUE)
		cat('*** Es altamente recomendable que utilices la matriz de correlaciones policorica ***',fill=TRUE)
		cat('*** como matriz de estimacion del modelo. Incluye el argumento policorica=T      ***',fill=TRUE)
		cat('------------------------------------------------------------------------------------',fill=TRUE)
		cat('',fill=TRUE)
		}
		if(es.dicotomica & !tetracorica & is.na(matcor)[1]){
		cat('',fill=TRUE)
		cat('--------------------------------------------------------------------------------------',fill=TRUE)
		cat('*** WARNING. Tus datos parecen dicotomicos. Es altamente recomendable que utilices ***',fill=TRUE)
		cat('*** la matriz de correlaciones tetracorica como matriz de estimacion del modelo    ***',fill=TRUE)
		cat('*** Incluye el argumento tetracorica=T                                             ***',fill=TRUE)
		cat('--------------------------------------------------------------------------------------',fill=TRUE)
		cat('',fill=TRUE)
		}
		
		cat('',fill=TRUE)
		cat('-----------------------------------------------------------------------------------',fill=TRUE)
		cat(' ***             Analisis Paralelo para determinar el numero de factores        ***',fill=TRUE)
		cat('',fill=TRUE)
		fa.parallel(cor.dat,n.obs=size[1])
		cat('',fill=TRUE)
		cat('***              Repite el analisis incluyendo el argumento n.factores          ***',fill=TRUE)
		cat('*** Ej. analisis.factorial.fnc(datos, n.factores=3)                             ***',fill=TRUE)
		cat('-------------------------------------------------------------------------------------',fill=TRUE)
		return( )
	}

	if(tipo=='cp') {
		solu1=principal(cor.dat,n.factores,n.obs=size[1], rotate='none')
		if(n.factores >= 2){
			solu2=principal(cor.dat,n.factores,n.obs=size[1], rotate=rotacion)
			if(rota=='oblicua') kaiser_=round(kaiser(solu1, rotate=rotacion)$loadings,3)
		}else{
			solu2=solu1
		}
	}
	if(tipo=='af') {
		solu1=try(fa(cor.dat,n.factores,rotate='none'),silent=TRUE)
		if(n.factores >= 2){
			solu2=fa(cor.dat,n.factores, rotate=rotacion)
			if(rota=='oblicua') kaiser_=round(kaiser(solu1, rotate=rotacion)$loadings,3)
		}else{
			solu2=solu1
		}
	}

	# SOLUCIONES ROTADAS Y NO ROTADAS
 	load1=round(solu1$loadings, 3)
 	h=round(solu1$communality,3);
 	u=1-h
 	solu.no.rota=cbind(load1,comunalidad=h, unicidad=u)

	if(n.factores >=2) {
		load2=round(solu2$loadings, 3)
		nombres.fac=names(data.frame(unclass(load2)))
		h2=round(solu2$communality,3);
		u2=1-h2
		if(rotacion=='oblimin'){
			solu.rota=cbind(kaiser_,comunalidad=h2, unicidad=u2)
		}else{
			solu.rota=cbind(load2,comunalidad=h2, unicidad=u2)
		}
	}
	# FIN SOLUCIONES

	#AJUSTE
	fit=factor.fit(cor.dat,solu1$loadings)
	fit=paste(round(fit,2),'%',sep='')
	residuo=as.matrix(residuals(solu2))

	if(tipo=='cp'){
		tabla.1=extrae.tabla.res.fac.fnc(solu1,c(7+nvar,7+nvar+2),n.factores)
		if(n.factores >= 2){
			tabla.2=extrae.tabla.res.fac.fnc(solu2,c(7+nvar,7+nvar+2),n.factores)
			tabla.3=round(solu2$r.scores,3)
		}
	}
	if(tipo=='af'){
		if(.Platform$OS.type !='windows' & .Platform$GUI!='AQUA'){
		  vector=c(6+nvar,6+nvar+2)
		}else{
		  vector=c(6+nvar,6+nvar+2)
		}
		if(n.factores==1){
			tabla.1=extrae.tabla.res.fac.fnc(solu1,vector,n.factores)
		}else{
			tabla.1=extrae.tabla.res.fac.fnc(solu1,vector,n.factores)
		}
		if(n.factores >= 2){
			tabla.2=extrae.tabla.res.fac.fnc(solu2,vector,n.factores)
			if(rotacion=='oblimin' | rotacion=='promax' | rotacion=='simplimax'){
				tabla.3=round(solu2$Phi,3)
				dimnames(tabla.3)=list(names(tabla.2),names(tabla.2))
			}	
			if(rotacion=='varimax' | rotacion=='quartimax'){
				tabla.3=diag(1,n.factores)
				dimnames(tabla.3)=list(names(tabla.2),names(tabla.2))
			}
		}
	}	

	ev=data.frame(autovalores);
	row.names(ev)=paste('PC',1:length(variables),sep='')
	ev=t(ev)
        if(!hay.suj) size=size[2]
	if(n.factores ==1){
		estructura = round(as.data.frame(unclass(solu1$loadings)),3)
		solu2=solu1
		#ORDENAMOS LAS CARGAS FACTORIALES
		ordenado=fa.sort(solu2)$loadings
		ordenado=round(as.data.frame(unclass(ordenado)),3)
		fs.r=try(data.frame(factor.scores(cor.dat,solu2)[[2]]),silent=TRUE)
		if(class(fs.r)=='try-error'){
			cat('',fill=TRUE)
			cat('*** WARNING. No ha sido posible extraer los pesos factoriales rotados ***',fill=TRUE)
			cat('*** se presentan los no rotados ***',fill=TRUE)
			fs.r=data.frame(unclass(solu2$weights))
		}
		names(fs.r)=names(estructura)
		lista=list(analisis=label.tipo,Sujetos.variables=size,
			missing=que.mis,
			var.NA=var.na,
			normalidad.multivariada=mardia_,
			Bartlett=barlett,
			KMO=kmo$KMO,
			MSA=kmo$MSA,
			RMS=kmo$RMS,
			Ajuste=fit,
			matriz.residual=residuo,
			rotacion='ninguna', valores.propios=ev,
			solucion.no.rotada=solu.no.rota,
			solucion.rotada.ordenada=ordenado,
			Varianzas=tabla.1,
			Pesos.factoriales=round(fs.r,4))
	}else{
		estructura = round(as.data.frame(unclass(solu2$loadings)),3)
		#ORDENAMOS LAS CARGAS FACTORIALES
		if(rota=='oblicua'){
			ordenado=fa.sort(kaiser_)
		}else{
			ordenado=fa.sort(solu2)$loadings
			ordenado=round(as.data.frame(unclass(ordenado)),3)
		}
		fs.r=try(data.frame(factor.scores(cor.dat,solu2)[[2]]),silent=TRUE)
		if(class(fs.r)=='try-error'){
			cat('',fill=TRUE)
			cat('*** WARNING. No ha sido posible extraer los pesos factoriales rotados ***',fill=TRUE)
			cat('*** se presentan los no rotados ***',fill=TRUE)
			fs.r=data.frame(unclass(solu2$weights))
		}
		names(fs.r)=names(estructura)
		lista=list(analisis=label.tipo,Sujetos.variables=size,
			missing=que.mis,
			var.NA=var.na,
			normalidad.multivariada=mardia_,
			Bartlett=barlett,
			KMO=kmo$KMO,
			MSA=kmo$MSA,
			RMS=kmo$RMS,
			Ajuste=fit,
			matriz.residual=residuo,
			rotacion=rotacion,valores.propios=ev,
			solucion.no.rotada=solu.no.rota,
			Varianzas.sin.rotacion=tabla.1,
			solucion.rotada=solu.rota,
			solucion.rotada.ordenada=ordenado,
			Varianzas.con.rotacion=tabla.2,
			Correlacion.entre.factores=tabla.3,
			Pesos.factoriales=round(fs.r,4))

	} #FIN SI N.FACTORES

	if(grafica){
		if(n.factores >= 2){
			grafi=try(grafica.componentes.fnc(estructura,tipo),silent=TRUE)
			if(class(grafi)=='try-error') {
				X11(); print(grafi[1])
			}else{
				print(grafi)
			}
		}
		if(tipo=='cp') label1='Componentes Principales. '
		if(tipo=='af') label1='Analisis Factorial. '
		if(n.factores==1)	titulo=label1
		if(n.factores > 1) titulo=paste(label1,' Rotacion ',rotacion,sep='')
		if(n.factores >= 1){
			X11( );
	 			print(fa.diagram(solu2,simple=FALSE, cut=corte, cex=size.font, 
					main=titulo))
		}
	}

	print(lista)
	if(latex) latex.fnc(lista[-c(1:4,6,7,9:13)])
	
	if(es.ordinal & !policorica & is.na(matcor)[1][1]){
		cat('',fill=TRUE)
		cat('------------------------------------------------------------------------------------',fill=TRUE)
		cat('*** WARNING. Tus datos parecen ordinales y tienen menos de 10 valores posibles.  ***',fill=TRUE)
		cat('*** Es altamente recomendable que utilices la matriz de correlaciones policorica ***',fill=TRUE)
		cat('*** como matriz de estimacion del modelo. Incluye el argumento policorica=T      ***',fill=TRUE)
		cat('------------------------------------------------------------------------------------',fill=TRUE)
		cat('',fill=TRUE)
	}
	if(es.dicotomica & !tetracorica & is.na(matcor)[1]){
		cat('',fill=TRUE)
		cat('--------------------------------------------------------------------------------------',fill=TRUE)
		cat('*** WARNING. Tus datos parecen dicotomicos. Es altamente recomendable que utilices ***',fill=TRUE)
		cat('*** la matriz de correlaciones tetracorica como matriz de estimacion del modelo    ***',fill=TRUE)
		cat('*** Incluye el argumento tetracorica=T                                             ***',fill=TRUE)
		cat('--------------------------------------------------------------------------------------',fill=TRUE)
		cat('',fill=TRUE)
	}

	# SI GUARDA O NO PF
	if(guarda.pf){
		if(is.numeric(variables)){nombres=names(datos); variables=nombres[variables]}
 		punt= try(data.frame(as.matrix(scale(dat_)) %*% as.matrix(fs.r)),silent=TRUE)
		if(tipo=='cp'){
			suma.pesos=apply(fs.r,2,function(x) abs(sum(x)))
			punt.sc=data.frame((as.matrix(dat_)%*% as.matrix(fs.r))/suma.pesos)
		}
		if(class(punt)=='try-error'){
			cat('',fill=TRUE)
			cat('*** WARNING. No ha sido posible estimar las puntuaciones factoriales ***',fill=TRUE)
			cat('',fill=TRUE)
			punt=data.frame(matrix(NA,dim(dat_)[1],dim(estructura)[2]))
		}
		if(grafica) { 
			if(tipo=='cp'){
				X11(); print(multi.hist(punt)); 
				X11(); print(multi.hist(punt.sc)) 
			}
			if(tipo=='af'){
				X11(); print(multi.hist(punt)); 
			}
		}			
		dat_$sujeto=row.names(dat_)
		chivato=existe.variable.fnc(datos,names(estructura))
		if(chivato$cc==0) {
			names(punt)=paste(names(estructura),'.2',sep='')
			variables=c(variables,paste(names(estructura),'.2',sep=''))
		}else{
			names(punt)=names(estructura)
			variables=c(variables,names(estructura))
		}
		punt$sujeto=as.numeric(row.names(punt))
		new.dat=merge(datos,punt,by='sujeto',all.x=TRUE, all.y=TRUE, sort=FALSE)
		cat('',fill=TRUE)
		cat('*** Estos son los 6 primeros valores de las variables a analizar ***',fill=TRUE)
		cat('*** y de las puntuaciones factoriales generadas y guardadas.     ***',fill=TRUE)
		cat('',fill=TRUE)
		new.dat=new.dat[,-1]
	 	print(head(new.dat[,variables]))
		cat('',fill=TRUE)
		return(new.dat)
	}else{
		if(hay.suj){
			cat('*** Si deseas las puntuaciones factoriales por sujeto introduce el ***',fill=TRUE)
			cat('*** argumento guarda.pf=T                                          ***',fill=TRUE)
			cat('',fill=TRUE)
		}else{
			cat('*** Si deseas las puntuaciones factoriales por sujeto introduce los ***',fill=TRUE)
			cat('*** argumentos guarda.pf=T, simula=T, si omites n entonces n=200    ***',fill=TRUE)
			cat('',fill=TRUE)
		}
	} # FIN SI GUARDA PF
	try(detach(package:GPArotation), silent=TRUE)
	if(return.res & !guarda.pf) return(lista)
}
#---------------------------------------------------------------------------
# Extraida de la libreria rela. Estima KMO, MSA y RMS de cor.dat
#---------------------------------------------------------------------------
 paf2.fnc = function(cor.dat, eigcrit=1, convcrit=.001) {

 	x=cor.dat

	bt=NA
	options(digits=5)

	x = as.matrix(x)
	x1 = x; rownames(x1)=colnames(x)

	# Correlation matrix inverse
	invx = solve(x)

	# S-squared matrix
	ssqr = matrix(0,nrow(x),ncol(x))
	diag(ssqr) = diag(invx)
	ssqr = solve(ssqr)

	# Partial correlations (anti-image)
	Q = ssqr%*%invx%*%ssqr
	colnames(Q) = colnames(x)
	rownames(Q) = colnames(x)

	# Anti-image correlation matrix
	qdi = diag(diag(Q))
	sqdi = solve(qdi)
	ssqdi = sqrt(sqdi)
	Qr = ssqdi%*%Q%*%ssqdi
	colnames(Qr) = colnames(x)
	rownames(Qr) = colnames(x)

	# KMO
	xnod = x
	diag(xnod) = 0
	Qrnod = Qr
	diag(Qrnod) = 0
	KMO = sum(xnod^2)/(sum(xnod^2)+sum(Qrnod^2))

	# MSA
	MSA = 0
	for (i in 1:nrow(xnod)) (MSA=c(MSA,sum(xnod[i,]^2)/(sum(xnod[i,]^2)+sum(Qrnod[i,]^2))))
	MSA= matrix(MSA[-1],,1)
	rownames(MSA) = colnames(x)
	colnames(MSA) = "MSA"

	# First communialities
	comm0 = 1-diag(Q)

	# Computing subsequent iterrations
	comm1 = comm0
	allcomm = 0
	diffscores = 0
	iter = 0
	count = 0
	eigenval = 0
	x0 = x

	repeat {

	allcomm = cbind(allcomm,comm1)

	eigs = 0
	diag(x) = comm1

	for (i in 1:length(eigen(x)$values))
		if (eigen(x0)$values[i] > eigcrit) {eigs = c(eigs,eigen(x)$values[i]) }

	eigs = eigs[-1]
	eigenval = cbind(eigenval, eigen(x)$values)

	eigmat = sqrt(diag(eigs, length(eigs), length(eigs)))

	eigvecr = matrix(eigen(x)$vector[,0:length(eigs)],,length(eigs))
	one = c((1:ncol(eigvecr))/(1:ncol(eigvecr)))

	factload = eigvecr%*%eigmat

	comm2 = t(rowsum(t(factload^2), one))
	dif = abs(comm1-comm2)

	iter = iter+1
	count = c(count,iter)
	diffscores = cbind(diffscores, dif)

	comm1 = comm2
	endtest = matrix(1, nrow(dif),1)
	for (i in 1:nrow(dif)) if (dif[i,1]<convcrit) {endtest[i,1]=NA}
	if (length(na.omit(endtest))==0) break }

	# Preparing and labeling output
	firstlast = cbind(comm0,comm1); colnames(firstlast)=c("Initial Communalities", "Final Extraction")
	allcomm = cbind(allcomm, comm1)
	allcomm = allcomm[,-1]; colnames(allcomm)=count
	diffscores = diffscores[,-1]; colnames(diffscores)=c(1:iter)
	eigenval = cbind(eigen(x0)$values, eigenval[,-1]); colnames(eigenval)=c(0:iter)

 	rownames(factload) = colnames(x)
 	facttest = factload[,1]
 	for (i in 1:length(facttest)) if (facttest[i]<0) {facttest[i]=NA}
 	if (length(na.omit(facttest))==0) (factload[,1] = -factload[,1]) 

	correp = factload %*% t(factload)
	residuals = x-correp
	colnames(correp) = colnames(x)
	rownames(correp) = colnames(x)
	colnames(residuals) = colnames(x)
	rownames(residuals) = colnames(x)
	diag(correp) = 1
	diag(residuals) = 0
	RMS = sqrt(sum(residuals^2)/((nrow(x)^2)-(nrow(x))))

	output = list("PRINCIPAL AXIS FACTORING", Correlation=x1, Anti.Image.Cov=Q,
	Anti.Image.Cor=Qr, KMO=KMO, MSA=MSA, 
	Bartlett=bt, Communalities=firstlast, Iterations=iter, Eigenvalues=eigenval, 
	Communality.Iterations=allcomm, Criterion.Differences=diffscores, Factor.Loadings=factload, Reproduced.Cor=correp,
	Residuals=residuals, RMS=RMS)

	ob = as.character(match.call()[2])
	cl = call("paf","object"=ob,"eigcrit"=eigcrit, "convcrit"=convcrit)

	output$call = cl
	output$items = names(output)

	class(output) = c("paf", class(output))
	output
}
#---------------------------------------------------------------------------


#---------------------------------------------------------------------------
# simula.factorial.fnc(cargas, phi, n)
#---------------------------------------------------------------------------

# cargas=c(0.9, 0.2,
#	  0.8, 0.2,
#	  0.6, 0.6,
#	  0.2, 0.8,
#	  0.2, -0.7)
# corr=c(1,0.4,
#       0.4,1)
#
 simula.factorial.fnc=function(cargas,corr,n){
      n.var=length(cargas)/2
      n.fac=length(cargas)/n.var
      cargas=matrix(cargas,nrow=n.var, byrow=TRUE)
      corr=matrix(corr, nrow=n.fac, byrow=TRUE)
      Sigma=cargas%*%corr%*%t(cargas)
      diag(Sigma)=1
      Sigma=try(cor.smooth(Sigma),silent=T)
      datos=try(data.frame(mvrnorm(n,rep(0,n.var), Sigma=Sigma)),silent=TRUE)
      if(class(datos)=='try-error'){
	cat('',fill=TRUE)
	cat('*** La matriz Sigma generada no es positiva definida. Prueba con otros valores ***',fill=TRUE)
	cat('',fill=TRUE)
	stop(datos)
      }
      crea.cat.fnc('SIMULADOR ANALISIS FACTORIAL')
      cat('', fill=TRUE)
      cat('*** Se ha creado una matriz de datos de',n,'observaciones en',n.var,'variables ',fill=TRUE)
      cat('*** a partir de las siguientes matrices de cargas y de correlacion entre factores ',fill=TRUE)
      cat('', fill=TRUE)
      print(list(Cargas=cargas, cor.entre.fact=corr))
 return(datos)
}
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# Extrae tabla valores propios y varianzas de modelo afactorial estimado
#---------------------------------------------------------------------------
 extrae.tabla.res.fac.fnc=function(solucion,vector,n.factores){
	sink('tmp.mod.nulo.txt')
	print(solucion)
	sink()
	if(n.factores==1) vector[2]=vector[1]+1
	raw01 = scan("tmp.mod.nulo.txt", sep="\n",what=character(), quiet=TRUE)
	nvar=dim(unclass(solucion$loading))[1]
	nombres.fac=names(data.frame(unclass(solucion$loadings)))
	ini=vector[1]; fin=vector[2]
	x= list(raw01[ini:fin])
	if(n.factores >=2) {
		x.1=strsplit(x[[1]][1], 'SS loadings')[[1]][2]
		x.2=strsplit(x[[1]][2], 'Proportion Var')[[1]][2]
		x.3=strsplit(x[[1]][3], 'Cumulative Var')[[1]][2]
		x.1=na.omit(as.numeric(strsplit(x.1,' ')[[1]][-c(1:4)]))
		x.2=na.omit(as.numeric(strsplit(x.2,' ')[[1]][-1]))
		x.3=na.omit(as.numeric(strsplit(x.3,' ')[[1]][-1]))
		tabla=data.frame(rbind(x.1,x.2,x.3))
		names(tabla)=nombres.fac
		row.names(tabla)=c('valor.propio','%.varianza','varianza.acumulada')
	}else{
		x.1=strsplit(x[[1]][1], 'SS loadings')[[1]][2]
		x.2=strsplit(x[[1]][2], 'Proportion Var')[[1]][2]
		x.1=as.numeric(strsplit(x.1,' ')[[1]][-c(1:4)])
		x.2=as.numeric(strsplit(x.2,' ')[[1]][-1])
		tabla=data.frame(rbind(x.1,x.2))
		names(tabla)=nombres.fac
		row.names(tabla)=c('valor.propio','%.varianza')
	}
 return(tabla)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# A partir de las cargas de las variables en los r factores, se plotean
# par a par las J(J-1)/2 componentes. Recibe la data.frame de la funcion
# analisis.factoria.fnc( )
#---------------------------------------------------------------------------
 grafica.componentes.fnc=function(estructura,tipo){
	J=dim(estructura)[2]; n.comp=J*(J-1)/2
	if(J==2) fc=c(1,1)
	if(J==3) fc=c(2,2)
	if(J==4) fc=c(3,2)
	if(J==5) fc=c(2,5)
	if(J > 5) { J=5; fc=c(2,5)}
	variables=names(estructura)
	variables=variables[1:J]
	Xi=matrix(c(1:length(variables)),1,length(variables))
	if(length(variables)==2) pares= t(matrix(tot.pares.fnc(Xi))) else
		pares=tot.pares.fnc(Xi)
	if(tipo=='cp') corte=0.4 else corte=0.2
	criterio= abs(estructura >= corte)
	par(mfrow=fc)
	for(i in 1:dim(pares)[1]){
	  dat.plot=estructura[,pares[i,]]
	  crit_=criterio[,pares[i,]]
	  c1=1+as.numeric(crit_[,1]==1)
	  c2=1+as.numeric(crit_[,2]==1)
	  color=(c1+c2)
	  titulo= paste('Componentes:',names(dat.plot)[1],names(dat.plot)[2],sep=' ')
	  plot(dat.plot, type='n', main=titulo)
	  text(dat.plot, row.names(dat.plot), col= color)
	}
	par(mfrow=c(1,1))
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# regresion.multiple.fnc(datos,que.vd,paso.a.paso=T, grafica=FALSE)
# Los datos de entrada deben incluir la variable dependiente y las vi
# seleccionadas de la matriz de datos global.
#---------------------------------------------------------------------------
regresion.multiple.fnc=function(datos=NA, variables=NA,vd,paso.a.paso=TRUE,exploratorio=FALSE,interaccion=FALSE,
                                grafica=FALSE,jerarquica=NA, dominancia=FALSE, ajustada.por=NA, robusta=FALSE,
                                latex=FALSE, via.step=FALSE){
  
  if(class(try(is.na(datos)))=='logical') {
    cat('',fill=TRUE)
    crea.cat.fnc('regresion.multiple.fnc')
    cat('---------------------------------------------------------------------', fill=TRUE)
    cat('*** Paso a paso ***', fill=TRUE)
    cat("  regresion.multiple.fnc(zprostate, variables=1:9, vd='lpsa')            ",fill=TRUE)
    cat('', fill=TRUE)
    cat('*** Simultanea ***', fill=TRUE)
    cat("  regresion.multiple.fnc(zprostate, variables=1:9, vd='lpsa',            ",fill=TRUE)
    cat("  			paso.a.paso=F)                                                ",fill=TRUE)
    cat('', fill=TRUE)
    cat('*** Eliminando casos extremos ***', fill=TRUE)
    cat("  regresion.multiple.fnc(zprostate[-c(38,39,81,95,96),], variables=1:9)  ",fill=TRUE)
    cat("  			vd='lpsa', grafica=T)                                         ",fill=TRUE)
    cat('', fill=TRUE)
    cat('*** Estimacion Jerarquica ***', fill=TRUE)
    cat("  regresion.multiple.fnc(zprostate[-c(38,39,81,95,96),], variables=1:9)  ",fill=TRUE)
    cat("  			vd='lpsa', jerarquica=c('lcavol','lweight','svi') )           ",fill=TRUE)
    cat('', fill=TRUE)
    cat('---------------------------------------------------------------------', fill=TRUE)
    cat(' Realiza analisis de regresion multiple con estimacion paso a paso o ', fill=TRUE)
    cat(' simultanea. El usuario puede ademas realizar la estimacion en modo  ', fill=TRUE)
    cat(' jerarquico a partir del orden de entrada explicitado en el argumento', fill=TRUE)
    cat(' jerarquica.                                                         ', fill=TRUE)
    cat('', fill=TRUE)
    cat(" sites.google.com/site/ullrtoolbox/regresion					", fill=TRUE)
    cat('---------------------------------------------------------------------', fill=TRUE)
    print(argumentos('regresion.multiple.fnc'))
    return('help')
  }
  
  if(grafica) exploratorio=TRUE else exploratorio=FALSE
  dat=datos
  nombres=names(dat)
  if(robusta & !interaccion) {
    crea.cat.fnc('ANALISIS DE REGRESION - ESTIMACION ROBUSTA (estimador M de Huber)')
  }else{
    crea.cat.fnc('ANALISIS DE REGRESION')
  }
  
  if(paso.a.paso & !interaccion) require(bestglm, quietly = TRUE); 
  if(interaccion){
      require(sjPlot, quietly=TRUE)
      cat('',fill=TRUE)
      cat('*** WARNING. Has solicitado un modelo con interaccion. Es muy importante',fill=TRUE)
      cat('*** que las variables cuantitativas entren centradas. Para ello puedes',fill=TRUE)
      cat('*** utilizar la funcion centra.variable.fnc.',fill=TRUE)
      cat('*** Si quisieras centrar las variables 2,3,4 y 6 puedes hacerlo asi:',fill=TRUE)
      cat('*** Ej: for(i in c(2:4,6)) datos=centra.variables.fnc(datos, variable=i)',fill=TRUE)
      cat('',fill=TRUE)
  }
  require(car, quietly = TRUE)
  if(dominancia) paso.a.paso=FALSE
  
  if(is.numeric(vd)) vd=nombres[vd]
  col.vd=match(vd,nombres)
  
  # CHECK SI EXISTE VD
  if(is.na(col.vd)){
    cat('',fill=TRUE)
    cat('***                        ERROR                        ***',fill=TRUE)
    cat('*** Tus datos no contienen la variable dependiente',vd,'***',fill=TRUE)
    cat("*** introduce el argumento vd. Ej. vd='peso'            ***",fill=TRUE)
    stop( )
  }
  # FIN SI EXISTE VD
  
  # SI EXISTE VARIABLES
  if(!is.na(variables[1])){
    if(is.numeric(variables)) variables=nombres[variables]
    mas.var=sum(is.na(variables))
    if(mas.var != 0){
      cat('',fill=TRUE)
      etiqueta=paste('*** Error. Has indicado un numero de variables superior al disponible ***')
      cat(etiqueta,fill=TRUE)
      cat('',fill=TRUE)
      stop( )
    }  
    vd.in.var=match(vd,variables)
    if(!is.na(vd.in.var)) variables=variables[-vd.in.var]
    chiv=match(variables,nombres)
    if(sum(is.na(chiv)) !=0){
      cat('',fill=TRUE)
      cat('*** Error. No existe en la base de datos incluida, la variable o variables:',fill=TRUE)
      cat('***          ',na.omit(variables[chiv]),'.Revisa tus datos',fill=TRUE)
      cat('',fill=TRUE)
      stop( )
    }
    dat=dat[,c(vd,variables)]
  }else{
    if(col.vd !=1) {
      f1=dat[,-col.vd]
      dat=cbind(dat[,vd],f1)
      variables=names(f1)
      names(dat)=c(vd,names(f1))
    }
  }
  # FIN SI EXISTE VARIABLES
  
  #------------------------------------------------------------------------
  # REGRESION JERARQUICA
  if(!is.na(jerarquica[1])){
    # CHECK SI is.numeric(jerarquica)
    if(is.numeric(jerarquica)){
      jerarquica_=nombres[jerarquica]
      indice=is.na(jerarquica)
      if(sum(indice)!=0){
        que.num=jerarquica[indice]
        cat('',fill=TRUE)
        cat('*** Error. La columna',que.num,'no existe en la base de datos  ***',fill=TRUE)
        cat('*** revisa los valores introducidos en el argumento jerarquica ***',fill=TRUE)
        cat('',fill=TRUE)
      }else{
        jerarquica=jerarquica_
      }
    }
    # FIN SI IS.NUMERIC
    
    paso.a.paso=FALSE
    chivato=match(jerarquica,names(dat))
    if(is.na(chivato[1])) {
      cat('*** ERROR. Has solicitado una regresion jerarquica, pero no has incluido    ***',fill=TRUE)
      cat('*** el vector de nombre jerarquica como argumento de la funcion, con los    ***',fill=TRUE)
      cat('*** nombres del orden en el que deseas que sean introducidas las variables. ***',fill=TRUE)
      cat("*** Ej.: variables=c('v10','v8','v14','v13') o variables=c(2,4,8,12:16)     ***",fill=TRUE)
      stop( )
    }
    
    crea.cat.fnc('Modo Jerarquico')
    
    acumula=character( )
    dat=na.omit(dat[,c(vd,jerarquica)])
    modelo=eval(parse(text=paste(vd,'~.',sep='')))
    lista=list( )
    list.mod=list( )
    deter=integer( )
    for(i in 1:length(jerarquica)){
      acumula= c(acumula,jerarquica[i])
      que.var=dat[,c(vd,acumula)]
      mod.lm=lm(modelo,data=que.var)
      list.mod[[i]]=mod.lm		  
      dat.z=que.var
      for(j in 1:dim(dat.z)[2])	dat.z[,j]=tipifica.fnc(que.var,j) 
      mod.lm.st=data.frame(round(lm(modelo,data=dat.z)$coef,3))
      mod.lm.st[1,1]=0
      names(mod.lm.st)='Beta'
      res=summary(mod.lm)
      coeficientes=round(res$coefficients,5)
      R=res$r.squared; R.adj=res$adj.r.squared
      deter[i]=R
      F.s=data.frame(t(res$fstatistic));
      names(F.s)=c('F','gl.1','gl.2')
      F.s$p.val=1-pf(F.s$F,F.s$gl.1,F.s$gl.2)
      row.names(F.s)="R2/(1-R2)"
      residuales=summary(res$residuals)
      parcial=try(cor.parcial.semiparcial.fnc(dat.z,acumula,vd),silent=TRUE)
      tabla=list(Variable.dependiente=vd,
                 Variables.seleccionadas= names(que.var)[-1],
                 Residuales=residuales,
                 Coeficientes=cbind(mod.lm.st,coeficientes),
                 Correlacion=parcial,
                 Indices=c(R=sqrt(R),R2=R,R2.adj=R.adj),Anova=F.s)
      lista[[i]]=tabla
    } # FIN LOOP POR VARIABLE
    names(list.mod)=jerarquica
    # Calculo resumen anova
    list.anova=list( )
    list.anova[[1]]=anova(list.mod[[1]])
    for(j in 2:length(list.mod))
      list.anova[[j]]=anova(list.mod[[j-1]],list.mod[[j]])
    names(list.anova)=paste('in.',jerarquica,sep='')	
    
    # Calculo Resumen Beta
    alma.beta=list( )
    for(i in 1:length(lista))
      alma.beta[[i]]=lista[[i]]$Coeficientes[,1]
    almacen=matrix(NA,length(alma.beta)+1,length(alma.beta))
    for(i in 1:dim(almacen)[2])
      almacen[1:(i+1),i]=as.numeric(alma.beta[[i]]$Beta)
    almacen=data.frame(almacen[-1,])
    row.names(almacen)=row.names( alma.beta[[length(alma.beta)]])[-1]
    names(almacen)=paste('Beta.p',1:length(alma.beta),sep='.')
    
    # Calculo Resumen indice de determinacion
    deter=data.frame(deter)
    dif=integer( )
    for(l in 1:dim(deter)[1]){
      if(l==1) dif[l]=0 else dif[l]=deter[l,]-deter[l-1,]
    }
    deter$incremento=round(dif,3)
    names(deter)=c('R.2','incremento')
    row.names(deter)=paste('Entra.',jerarquica,sep='')
    # Fin resumen
    
    names(lista)=paste('paso.',1:length(jerarquica),sep='')
    n.lista=length(lista)+1
    lista[[n.lista]]=almacen
    names(lista)[n.lista]='Resumen.Beta.por.pasos'
    n.lista=length(lista)+1
    lista[[n.lista]]=deter
    names(lista)[n.lista]='Resumen.R2.por.pasos'
    lista[[6]]=list.anova
    names(lista)[6]='Steps.anova'	   
    return(lista)
  } # FIN DEL PROCEDIMIENTO DE REGRESION JERARQUICA
  #------------------------------------------------------------------------
  
  #Chequea si hay factores
  es.factor=logical( )
  check=dat[,variables]
  
  if(is.null(dim(check))){
    paso.a.paso=FALSE
    es.factor=is.factor(check)
  }else{
    for (i in 1:dim(check)[2]) es.factor[i]=is.factor(check[,i]) | is.character(check[,i])
  }
  chivato.factor=sum(es.factor)
  
  # SI SE SOLICITA INTERACCION EN EL MODELO
  if(interaccion){
    modelo=eval(parse(text=paste(vd,'~.*.',sep='')))
    mod.lm=lm(modelo, data=dat,na.action=na.omit, x=TRUE)
    cat('***                     Paso a paso                   ***',fill=TRUE)
    mod.lm=step(mod.lm)
    paso.a.paso=FALSE 
    dominancia=FALSE; jerarquica=NA
  }
  # FIN SI SE SOLICITA INTERACCION EN EL MODELO
  
  # PASO A PASO
  
	if(via.step){
      	modelo=eval(parse(text=paste(vd,'~.',sep='')))
      	n.previo=dim(dat)[1]
      	dat=na.omit(dat)
      	n.pos=dim(dat)[1]
    	mod.lm=lm(modelo, data=dat,na.action=na.omit, x=TRUE)
    	cat('***                            Paso a paso                       ***',fill=TRUE)
    	cat('*** El procedimiento paso a paso requiere eliminar los sujetos   ***',fill=TRUE)
    	cat('*** con valores perdidos.                                        ***',fill=TRUE)
    	etiq=paste('*** N con missing: ',n.previo,'         N sin missing: ',n.pos,sep='')
    	cat(etiq, fill=TRUE)
    	cat('',fill=TRUE)
    	mod.lm=step(mod.lm)
    	coe=summary(mod.lm)$coeff[-1,]
    	indice=coe[,4] <= 0.05
    	new.var=row.names(coe[indice,])
    	dat=dat[,c(vd,new.var)]
    	mod.lm=lm(modelo, data=dat,na.action=na.omit, x=TRUE)
   	    paso.a.paso=FALSE
	}  
  
  if(paso.a.paso){  
    # Chequea si hay factores
    if(sum(es.factor)!=0){
      var.fac=variables[es.factor]
      cat('',fill=TRUE)
      cat('*** Hay variables categoricas en el modelo, utiliza la regresion ***',fill=TRUE)
      cat('*** logistica. La variable ',var.fac,' es categorica.',fill=TRUE)
      cat('*** Las variables categoricas seran eliminadas del procedimiento ***',fill=TRUE)
      cat('*** paso a paso solicitado. Si deseas incluirla/s, en el modelo  ***',fill=TRUE)
      cat('*** indica paso.a.paso=FALSE en la funcion.                          ***',fill=TRUE)
      cat('',fill=TRUE)
    } # FIN DE CHECK SI HAY FACTORES
    
    require(bestglm, quietly=TRUE)
    dat=na.omit(dat)
    y=dat[,1]
    X=dat[,-1]
    Xy  = cbind(as.data.frame(X),y)
    out = try(bestglm(Xy),silent=TRUE)
    if(class(out)=='try-error'){
      cat('', fill=TRUE)
      cat('*** Error. Ha habido un problema con la estimacion paso a paso. Incluye el', fill=TRUE)
      cat('*** argumento paso.a.paso=F en la llamada a la funcion.', fill=TRUE)
      cat('', fill=TRUE)
      stop( )	
    }	
    p.a.p=out$BestModels
    
    selecc=names(coef(out$BestModel))[-1]
    X=data.frame(X[,selecc])
    nombres=selecc
    dat=cbind(y,X)
    if(dim(dat)[2]==1){
      cat('*** Ninguna variable ha resultado significativa en el procedimiento paso a paso ***',fill=TRUE)
      stop( )
    }
    names(dat)=c(vd,nombres)
  }else{
    nombres=names(dat[,-1])
  } # FIN DEL PASO A PASO
  
  # SI NO INTERACCION
  if(!interaccion){
      modelo=eval(parse(text=paste(vd,'~.',sep='')))
      if(!robusta){
	  mod.lm=lm(modelo,data=dat) 
      }else{
	  mod.lm=rlm(modelo, data=dat)
	  dat_=dat
	  dat_$w=mod.lm$w
	  dat_=dat_[dat_$w < 1,]
	  dat_=ordena.por.variable.fnc(dat_,'w',silente=TRUE)
	  cat('*** Casos con ponderacion (w) inferior a 1 ***',fill=TRUE)
	  cat('',fill=TRUE)    
	  print(dat_)
	  plot(mod.lm$w, pch=20, ylab='w', main='Vector de pesos (w) por caso')
	  abline(h=0.7, lty=3, col=2)
	  x11()  
	}	   
  }
  # FIN TIPO DE MODELO NO INTERACTIVO
  
  n=dim(na.omit(dat))[1]
  Akaike=AIC(mod.lm); Bayes=BIC(mod.lm)
  nvar=(dim(dat)[2])-1
  
  #----------------------------------------------------------------
  # ANALISIS DE DOMINANCIA
  #----------------------------------------------------------------
  if(nvar > 1 & dominancia){
    if(!is.na(ajustada.por[1])){
      indice1=ajustada.por %in% names(datos)
      if(sum(indice1) != length(ajustada.por)){
        falta=ajustada.por[!indice1]
        cat('*** Error. No existe la variable:',falta,'que has incluido en el ',fill=TRUE)
        cat('*** argumento ajustada.por para realizar el analisis de dominancia     ',fill=TRUE)
        cat('*** Revisa el contenido de dicho argumento.                      ',fill=TRUE)
        cat(' ',fill=TRUE)
        stop( )
      }
    }
    domi=dominancia.lm.fnc(mod.lm, dat,ajustada.por=ajustada.por)
  }
  #----------------------------------------------------------------
  
  dwatson=durbinWatsonTest(mod.lm,alternative=c("two.sided"),max.lag=1)
  
  # COLINEALIDAD SI N.PRED > 1
  if(dim(dat)[2] > 2){
    Colinealidad=cbind(VIF=vif(mod.lm),tolerancia=1/vif(mod.lm))
  }else{
    Colinealidad=c(VIF=NA,Tolerancia=NA)
  }
  
  pacf(resid(mod.lm),lag.max=1,main='Autocorrelacion parcial de residuales')
  X11()
  
  # EXPLORATORIO DEL MODELO ESTIMADO
  transforma=try(powerTransform(mod.lm,family='yjPower'),silent=TRUE)
  cat('',fill=TRUE)
  cat('*** Transformacion de Potencia de la variable criterio',vd,'para promover normalidad',fill=TRUE)
  cat('',fill=TRUE)
  print(summary(transforma))
  cat('',fill=TRUE)
  cat('*** Transformacion de Potencia de la variable criterio',vd,'para promover',fill=TRUE)
  cat('*** varianza constante. Test Breusch-Pagan',fill=TRUE)
  cat('',fill=TRUE)
  
  sal.nvc=(try(ncvTest.lmB(mod.lm,dat=dat),silent=TRUE))
  if(class(sal.nvc)!='try-error') print(sal.nvc)
  
  vc=try(spreadLevelPlot(mod.lm, main='Dispersion de Residuales vs Prediccion'),silent=TRUE)
  if(class(vc)!='try-error'){
    cat('Transpormacion de potencia sugerida:',vc$PowerTransformation,fill=TRUE)
    #        	X11( )
    cat('',fill=TRUE)
  }
  
  # CALCULO DE BETA
  if(chivato.factor==0){
    if(dim(dat)[2] > 2){
      parcial=try(cor.parcial.semiparcial.fnc(dat,nombres,vd),silent=TRUE)
      if(class(parcial)=='try-error') parcial=NA
    }else{
      parcial=NA
    }
    if(!interaccion){
      dat.z=dat
      for(i in 1:dim(dat)[2])	dat.z[,i]=tipifica.fnc(dat,i)
      mod.lm.st=data.frame(round(lm(modelo,data=dat.z)$coef,3))
      mod.lm.st[1,1]=0
      names(mod.lm.st)='Beta'
    }else{
      mod.lm.st=try(sju.betaCoef(mod.lm),silent=TRUE); mod.lm.st=data.frame(Beta=mod.lm.st)
      n.par=dim(mod.lm.st)[1]+1
      mod.lm.st[n.par,1]=0
      nomb_=row.names(mod.lm.st); nomb_[n.par]='(Intercept)'; row.names(mod.lm.st)=nomb_
      mod.lm.st$num=c(2:n.par,1); 
      mod.lm.st=suppressWarnings(ordena.por.variable.fnc(mod.lm.st,'num', silente=TRUE))
      mod.lm.st=data.frame(Beta=mod.lm.st[,1]); mod.lm.st$Beta=round(mod.lm.st$Beta,4)
    }
  }
  # FIN CALCULO BETA

  if(chivato.factor !=0 & !paso.a.paso){
    cat('*** Dado que tienes variables categoricas en el modelo, no se presentara     ***',fill=TRUE)
    cat('*** la solucion estandarizada ni las correlaciones parciales y semiparciales ***',fill=TRUE)
    cat('', fill=TRUE)
  }
  
  # CREA TABLA DE RESULTADOS
  res=summary(mod.lm)
  coeficientes=round(res$coefficients,4)
  if(!robusta){
    R=res$r.squared; R.adj=res$adj.r.squared
    gl=as.numeric(res$fstatistic[3])
  }else{
    R=cor(fitted(mod.lm),datos[,vd])^2
    k=dim(coeficientes)[1]-1
    R.adj=1-(1-R)*((n-1)/(n-k-1))
    gl=unlist(summary(mod.lm))$df2
  }  
  coe=coeficientes[,1]
  et=coeficientes[,2]*qt(0.975,gl)
  ic.inf=coe-et
  ic.sup=coe+et
  IC=round(t(rbind(ic.inf,ic.sup)),4)
  coeficientes=cbind(coeficientes,IC)
  
  if(!robusta){
    F.s=data.frame(t(res$fstatistic));
    names(F.s)=c('F','gl.1','gl.2')
    F.s$p.val=1-pf(F.s$F,F.s$gl.1,F.s$gl.2)
    row.names(F.s)="R2/(1-R2)"
  }else{
    F.s=data.frame(F=(R*(n-k-1))/((1-R)*k))
    F.s$gl1=k; F.s$gl2=n-k-1
    F.s$p.val=1-pf(F.s$F,F.s$gl1,F.s$gl2); 
  }
  
  residuales=summary(res$residuals)
  
  if(chivato.factor==0){
    if(paso.a.paso){
      if(dominancia){
        tabla=list(modelo=mod.lm,Variable.dependiente=vd,
                   Paso.a.paso=p.a.p,
                   Variables.seleccionadas=data.frame(nombres),
                   Residuales=residuales,Durbin.Watson.test=dwatson,
                   Coeficientes=cbind(mod.lm.st,coeficientes),
                   AIC=c(AIC=Akaike, BIC=Bayes),
                   Colinealidad=Colinealidad,
                   Correlacion=parcial,
                   Indices=c(R=sqrt(R),R2=R,R2.adj=R.adj),Anova=F.s,
                   Dominancia=domi)
      }else{
        tabla=list(modelo=mod.lm,Variable.dependiente=vd,
                   Paso.a.paso=p.a.p,
                   Variables.seleccionadas=data.frame(nombres),
                   Residuales=residuales,Durbin.Watson.test=dwatson,
                   Coeficientes=cbind(mod.lm.st,coeficientes),
                   AIC=c(AIC=Akaike, BIC=Bayes),					
                   Colinealidad=Colinealidad,
                   Correlacion=parcial,
                   Indices=c(R=sqrt(R),R2=R,R2.adj=R.adj),Anova=F.s)
      }
    }else{
      if(dominancia){
        tabla=list(modelo=mod.lm,Variable.dependiente=vd,
                   Variables.seleccionadas=data.frame(nombres),
                   Residuales=residuales,Durbin.Watson.test=dwatson,
                   Coeficientes=cbind(mod.lm.st,coeficientes),
                   AIC=c(AIC=Akaike, BIC=Bayes),
                   Colinealidad=Colinealidad,
                   Correlacion=parcial,
                   Indices=c(R=sqrt(R),R2=R,R2.adj=R.adj),Anova=F.s,
                   Dominancia=domi)
      }else{
        tabla=list(modelo=mod.lm,Variable.dependiente=vd,
                   Variables.seleccionadas=data.frame(nombres),
                   Residuales=residuales,Durbin.Watson.test=dwatson,
                   Coeficientes=cbind(mod.lm.st,coeficientes),
                   AIC=c(AIC=Akaike, BIC=Bayes),
                   Colinealidad=Colinealidad,
                   Correlacion=parcial,
                   Indices=c(R=sqrt(R),R2=R,R2.adj=R.adj),Anova=F.s)
      }
    }
  }else{
    tabla=list(modelo=mod.lm,Variable.dependiente=vd,Variables.seleccionadas=data.frame(nombres),
               Residuales=residuales,Durbin.Watson.test=dwatson,Coeficientes=coeficientes,
               AIC=c(AIC=Akaike, BIC=Bayes),				
               Colinealidad=Colinealidad,
               Indices=c(R=sqrt(R),R2=R,R2.adj=R.adj),Anova=F.s)
  }
  
  if(interaccion) {
    tiempo=Sys.time()
	tiempo=strsplit(as.character(tiempo),' ')
	hora=strsplit(tiempo[[1]][2],':')[[1]]
	hora=paste(hora[1],hora[2],hora[3],sep='_')
	fecha=paste(tiempo[[1]][1],hora,sep='_')	
    nombre.pdf=paste('graf_interac_regres_',fecha,'.pdf',sep='')
    pdf(file=nombre.pdf, width=12, height=10, paper='a4r')
      no.interac=try(sjp.lm.int(mod.lm),silent=TRUE)
    dev.off( )
    n.elem=length(tabla)
    nomb.tab=names(tabla)
    if(class(no.interac)=='try-error'){
      v1='*** Ninguna interaccion ha resultado significativa      ***'
      v2='*** No se genera archivo de salida grafico              ***'
    }else{  
      v1='*** Se ha creado el archivo graf_interac_regresion.pdf con las graficas'
      v2='*** de las interacciones que han resultado significativas.'
    }  
    etiqu=rbind(v1,v2); row.names(etiqu)=c('',''); colnames(etiqu)=''
    tabla[[n.elem+1]]=etiqu
    nomb.tab[n.elem+1]='Graficas.Interaccion'
    names(tabla)=nomb.tab
  }    
    
  if(exploratorio){
    resumen=list()
    dat.z=dat
    for(i in 1:dim(dat)[2]) resumen[[i]]=summary(dat[,i])
    names(resumen)=names(dat)
    print(resumen)
    print(correlacion.fnc(dat))
    grafica.correlacion.fnc(dat)
    for(i in 1:dim(dat)[2]) dat.z[,i]=tipifica.fnc(dat,i)
    names(dat.z)=names(dat)
    X11( )
    boxplot(dat.z,main='Variables en puntuaciones Z')
    X11( )
    plot(hatvalues(mod.lm),rstudent(mod.lm),type='n',xlab='Leverage',ylab='zRes',
         main='z.Resid vs Leverage (Cook.d)')
    cook=sqrt(cooks.distance(mod.lm))
    points(hatvalues(mod.lm),rstudent(mod.lm), cex=5*cook/max(cook))
    text(hatvalues(mod.lm),rstudent(mod.lm), labels=row.names(dat))
    abline(h=c(-2,0,2), lty=2)
  }
  res.reg=summary(mod.lm)
  if(grafica) {
    X11( )
    par(mfrow=c(2,2))
    plot(mod.lm)
    par(mfrow=c(1,1))
  }
  if(!robusta){
    res.anova=Anova(mod.lm,type=3)	
  }else{
    res.anova=NA
  }
  if(latex) latex.fnc(tabla[-c(1:6)])
  try(detach(package:bestglm),silent=TRUE)
  try(detach(package:sjPlot),silent=TRUE)
  #try(detach(package:car),silent=TRUE)
  return(tabla)
}
#---------------------------------------------------------------------------

#----------------------------------------------------------------
# ANALISIS DE DOMINANCIA
#----------------------------------------------------------------
 dominancia.lm.fnc=function(modelo, dat, ajustada.por=NA){
 	require(relaimpo, quietly=TRUE)
	domi=calc.relimp(modelo,type=c('lmg','car','first','last'))
	if(!is.na(ajustada.por[1])){
		if(sum(ajustada.por %in% names(dat)) != length(ajustada.por))
			ajustada.por=ajustada.por[ajustada.por %in% names(dat)]
		domi=calc.relimp(modelo,type=c('lmg','first','last'),
			always=ajustada.por)
		 
	}
 	# FIN DOMINANCIA
 	try(detach(package:relaimpo),silent=TRUE)
 return(domi)
 }
#----------------------------------------------------------------

#---------------------------------------------------------------------------
# Revision history:
# 2009-09-28 by J. Fox (/renamed)
# 2012-07-01 Rewritted by S. Weisberg.  The 'data' gone
#---------------------------------------------------------------------------
# score test of nonconstant variance (J. Fox)
#---------------------------------------------------------------------------
ncvTest.lmB = function(model, var.formula,dat) {
	sumry = summary(model)
	residuals = residuals(model, type="pearson")
	S.sq = df.residual(model)*(sumry$sigma)^2/sum(!is.na(residuals))
	.U = (residuals^2)/S.sq
	if (missing(var.formula)) {
		mod = lm(.U ~ fitted.values(model))
		varnames = "fitted.values"
		var.formula = ~fitted.values
		df = 1
	}
	else 
  { 
   form = as.formula(paste(".U ~ ", as.character(var.formula)[[2]], sep=""))
   data = getCall(model)$data
   if(!is.null(data)){
        data = eval(data)
        data$.U = .U 
   }
   mod = update(model, form, data=data, weights=NULL) 
   df = sum(!is.na(coefficients(mod))) - 1    
  }	    
	SS = anova(mod)$"Sum Sq"
	RegSS = sum(SS) - SS[length(SS)]
	Chisq = RegSS/2
	result = list(formula=var.formula, formula.name="Variance", ChiSquare=Chisq, Df=df, 
		p=pchisq(Chisq, df, lower.tail=FALSE), test="Non-constant Variance Score Test")
	class(result) = "chisqTest"
	return(result)
}  
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# cor.parcial.semiparcial.fnc(datos,variables,vd)
#---------------------------------------------------------------------------
 cor.parcial.semiparcial.fnc=function(datos,variables,vd){
 	require(ppcor, quietly = TRUE)
	datos=na.omit(datos[,c(vd,variables)])
	semipar=data.frame( )
	cor.par=data.frame( )
	for(i in 2:(length(variables)+1)){
 		resto=datos[,-c(1,i)]
 		semipar=rbind(semipar,spcor.test(datos[,vd],datos[,i],resto))
 		cor.par=rbind(cor.par,pcor.test(datos[,vd],datos[,i],resto))
	}
	row.names(semipar)=variables
	row.names(cor.par)=variables
	semipar=round(semipar[,-6],3)
	semipar$sp2=round(I(semipar$estimate^2),3)
	semipar=semipar[,c(1,6,2:5)]
	parcial=round(cor.par[,-6],3)
 	lista=list(cor.parcial=parcial,cor.semiparcial=semipar)
	try(detach(package:ppcor),silent=TRUE)
 return(lista)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# analisis.exploratorio.fnc(datos.inter,fac.inter,fac.intra)
#---------------------------------------------------------------------------
 analisis.exploratorio.anova.fnc=function(datos,fac.inter=NA,fac.intra=NA){
	datos.inter=datos
  if(!is.na(fac.intra[1])) { n.fac.intra=length(fac.intra); names.intra=names(fac.intra)}
  if(!is.na(fac.inter[1])) n.fac.inter=length(fac.inter);
  require(lattice, quietly = TRUE)

 # SI NO HAY FAC.INTRA DEVUELVE NULL
 if(is.na(fac.intra[1])){
	cat('',fill=TRUE)
	cat('*** Error. Esta funcion requiere como minimo un factor intragrupo.  ***',fill=TRUE)
	cat('*** Si tu modelo es completamente aleatorizado, puedes utilizar las ***',fill=TRUE)
	cat('*** funciones histograma.fnc y diagrama.cajas.fnc si desear llevar  ***',fill=TRUE)
	cat('*** a cabo un analisis exploratorio grafico de tus datos.           ***',fill=TRUE)
 stop( )
 }

 if(is.na(fac.inter[1])){
	if(n.fac.intra ==1){
		modelo=eval(parse(text=
			paste('vd','~',names.intra[1],'|','sujeto',sep='')))
 		print(xyplot(modelo,type=c('g','a'),
			main=paste('sujeto:',names.intra[1],sep=''),
			data=datos.inter))
	}
	if(n.fac.intra ==2){
		modelo=eval(parse(text=
		paste('vd','~',names.intra[2],'|','sujeto*',names.intra[1],sep='')))
 		print(xyplot(modelo,type=c('g','a'),
		main=paste('sujeto:',names.intra[1],':',names.intra[2],sep=''),
		data=datos.inter))
	}
 }else{
	if(n.fac.inter == 1 & n.fac.intra==1){
		modelo=eval(parse(text=
			paste('vd','~',names.intra[1],'|','sujeto',sep='')))
 		print(xyplot(modelo,groups=eval(parse(text=fac.inter[1])),type=c('g','a'),
			main=paste('sujeto:',names.intra[1],':',fac.inter[1],sep=''),
			key = simpleKey(text = levels(datos.inter[,fac.inter[1]]),
			space = "right", points = TRUE),
			data=datos.inter))
	}
	if(n.fac.inter == 1 & n.fac.intra==2){
		X11()
		modelo=eval(parse(text=
			paste('vd','~',names.intra[2],'|','sujeto*',names.intra[1],sep='')))
 		print(xyplot(modelo,groups=eval(parse(text=fac.inter[2])),type=c('g','a'),
			main=paste('sujeto:',names.intra[2],':',names.intra[1],
			':',fac.inter[1],sep=''),
			key = simpleKey(text = levels(datos.inter[,fac.inter[1]]),
			space = "right", points = TRUE),
			data=datos.inter))
	}
	if(n.fac.inter == 2 & n.fac.intra==1){
		modelo=eval(parse(text=
			paste('vd','~',names.intra[1],'|','sujeto',sep='')))
 		print(xyplot(modelo,groups=eval(parse(text=fac.inter[1])),type=c('g','a'),
			main=paste('sujeto:',names.intra[1],':',fac.inter[1],sep=''),
			key = simpleKey(text = levels(datos.inter[,fac.inter[1]]),
			space = "right", points = TRUE),
			data=datos.inter))
		X11()
		modelo=eval(parse(text=
			paste('vd','~',names.intra[1],'|','sujeto',sep='')))
 		print(xyplot(modelo,groups=eval(parse(text=fac.inter[2])),type=c('g','a'),
			main=paste('sujeto:',names.intra[1],':',fac.inter[2],sep=''),
			key = simpleKey(text = levels(datos.inter[,fac.inter[2]]),
			space = "right", points = TRUE),
			data=datos.inter))
	}
	if(n.fac.inter == 2 & n.fac.intra==2){
		modelo=eval(parse(text=
			paste('vd','~',names.intra[2],'|','sujeto*',names.intra[1],sep='')))
		graf1=
 			xyplot(modelo,groups=eval(parse(text=fac.inter[1])),type=c('g','a'),
				main=paste('sujeto:',names.intra[1],':',fac.inter[1],sep=''),
				key = simpleKey(text = levels(datos.inter[,fac.inter[1]]),
				space = "right", points = TRUE),
				data=datos.inter)
		graf2=
 			xyplot(modelo,groups=eval(parse(text=fac.inter[2])),type=c('g','a'),
				main=paste('sujeto:',names.intra[1],':',fac.inter[2],sep=''),
				key = simpleKey(text = levels(datos.inter[,fac.inter[2]]),
				space = "right", points = TRUE),
				data=datos.inter)
		print(graf1)
		X11()
		print(graf2)

		modelo.bx=eval(parse(text=
			paste('vd','~',names.intra[2],'|',names.intra[1],sep='')))
		by.vi1=split(datos.inter,datos.inter[,fac.inter[1]])
		by.vi2=split(datos.inter,datos.inter[,fac.inter[2]])
		for(i in by.vi1){
			X11()
 			grafi=bwplot(modelo.bx, type=c('g','a'),layout=c(3,1),
				main=as.character(unique(i[,fac.inter[1]])),data=i)
			print(grafi)
		}
		for(i in by.vi2){
			X11()
 			grafi=bwplot(modelo.bx, type=c('g','a'),layout=c(3,1),
				main=as.character(unique(i[,fac.inter[2]])),data=i)
			print(grafi)
		}
	}
 }
}
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# BoxMTest(dat[,covariantes],dat[,grupo])
#---------------------------------------------------------------------------
 BoxMTest <- function(X, cl) {
  ##
  ## Created by A. Trujillo-Ortiz and R. Hernandez-Walls
  ## Facultad de Ciencias Marinas
  ## Universidad Autonoma de Baja California
  ## Apdo. Postal 453
  ## Ensenada, Baja California
  ## Mexico.
  ## atrujo_at_uabc.mx
  ## And the special collaboration of the post-graduate students of the 2002:2
  ## Multivariate Statistics Course: Karel Castro-Morales,
  ## Alejandro Espinoza-Tenorio, Andrea Guia-Ramirez, Raquel Muniz-Salazar,
  ## Jose Luis Sanchez-Osorio and Roberto Carmona-Pina.
  ## November 2002.
  ##
  ## To cite this file, this would be an appropriate format:
  ## Trujillo-Ortiz, A., R. Hernandez-Walls, K. Castro-Morales,
  ## A. Espinoza-Tenorio, A. Guia-Ramirez and R. Carmona-Pina. (2002).
  ## MBoxtest: Multivariate Statistical Testing for the Homogeneity of
  ## Covariance Matrices by the Box's M. A MATLAB file. [WWW document].
  ## URL http://www.mathworks.com/matlabcentral/fileexchange/loadFile.do?objectId=273 3&objectType=FALSEILE
  ##
  ## References:
  ##
  ## Stevens, J. (1992), Applied Multivariate Statistics for Social Sciences.
  ## 2nd. ed., New-Jersey:Lawrance Erlbaum Associates Publishers. pp. 260-269.
  if(!is.factor(cl)) cl=as.factor(cl)
  g = nlevels(cl) ## Number of groups.
  n = table(cl) ## Vector of groups-size.
  N = nrow(X)
  p = ncol(X)
  bandera = 2
  if (any(n >= 20)) bandera = 1
  ## Partition of the group covariance matrices.
  covList=lapply(split(X,cl),function(x) var(x))

  deno = sum(n) - g
  suma = array(0, dim=dim(covList[[1]]))
  for (k in 1:g)
    suma = suma + (n[k] - 1) * covList[[k]]
  Sp = suma / deno ## Pooled covariance matrix.
  Falta=0
  for (k in 1:g)
    Falta = Falta + ((n[k] - 1) * log(det(covList[[k]])))

  MB = (sum(n) - g) * log(det(Sp)) - Falta ## Box's M statistic.
  suma1 = sum(1 / (n[1:g] - 1))
  suma2 = sum(1 / ((n[1:g] - 1)^2))
  C = (((2 * p^2) + (3 * p) - 1) / (6 * (p + 1) * (g - 1))) *
    (suma1 - (1 / deno)) ## Computing of correction factor.
  if (bandera == 1) {
    X2 = MB * (1 - C) ## Chi-square approximation.
    v = as.integer((p * (p + 1) * (g - 1)) / 2) ## Degrees of freedom.
    ## Significance value associated to the observed Chi-square statistic.
    P = round(pchisq(X2, v, lower=FALSE),6)

	salida=data.frame(MBox=MB, Chi.Sq=X2, gl=v, p.val=P)
	row.names(salida)=''
    return(salida)
  } else {
    ## To obtain the F approximation we first define Co, which combined to
    ## the before C value are used to estimate the denominator degrees of
    ## freedom (v2); resulting two possible cases.
    Co = (((p-1) * (p+2)) / (6 * (g-1))) * (suma2 - (1 / (deno^2)))
    if (Co - (C^2) >= 0) {
      v1 = as.integer((p * (p + 1) * (g - 1)) / 2) ## Numerator DF.
      v21 = as.integer(trunc((v1 + 2) / (Co - (C^2)))) ## Denominator DF.
      F1 = MB * ((1 - C - (v1 / v21)) / v1) ## F approximation.
      ## Significance value associated to the observed F statistic.
      P1 = pf(F1, v1, v21, lower=FALSE)
	 salida=data.frame(MBox=MB, Aprox.F=FALSE,gl1=v1,gl2=v21, p.val=P1)
	 row.names(salida)=''
      return(salida)
    } else {
      v1 = as.integer((p * (p + 1) * (g - 1)) / 2) ## Numerator df.
      v22 = as.integer(trunc((v1 + 2) / ((C^2) - Co))) ## Denominator df.
      b = v22 / (1 - C - (2 / v22))
      F2 = (v22 * MB) / (v1 * (b - MB)) ## F approximation.
      ## Significance value associated to the observed F statistic.
      P2 = pf(F2, v1, v22, lower=FALSE)
	 salida=data.frame(MBox=MB, Aprox.F=FALSE,gl1=v1,gl2=v22, p.val=P2)
	 row.names(salida)=''
      return(salida)
    }
  }
}
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# ANALISIS DISCRIMINANTE
# analisis.discriminante.fnc(datos,grupo,paso.a.paso=T)
#---------------------------------------------------------------------------
 analisis.discriminante.fnc=function(datos=NA, variables=NA,grupo, paso.a.paso=TRUE, VC=NA,
	grafica=FALSE, ordenado=TRUE, roc=FALSE, graf.cor=FALSE, map=FALSE, ADC=FALSE, 
	silente=FALSE, latex=FALSE){

	if(class(try(is.na(datos)))=='logical') {
		cat('',fill=TRUE)
		crea.cat.fnc('analisis.discriminante.fnc')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat('*** Paso a paso ***', fill=TRUE)
		cat("  modelo1= analisis.discriminante.fnc(iris, grupo='Species', grafica=T)    ",fill=TRUE)
		cat("  modelo1= analisis.discriminante.fnc(iris, variables=1:4, grupo='Species')",fill=TRUE)
		cat('', fill=TRUE)
		cat('*** Simultanea ***', fill=TRUE)
		cat("  modelo1= analisis.discriminante.fnc(iris, grupo='Species', paso.a.paso=F)",fill=TRUE)
		cat('', fill=TRUE)
		cat('*** Puntuaciones discriminantes ***', fill=TRUE)
		cat("  iris.2 = puntuacion.discriminante.fnc(iris, modelo=modelo1)              ",fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Realiza analisis discriminante lineal sobre el grupo de variables   ', fill=TRUE)
		cat(' explicitado por el usuario. Por defecto se lleva a cabo una estimacion', fill=TRUE)
		cat(' paso a paso. Si se incluye el argumento paso.a.paso=F se realizara de ', fill=TRUE)
		cat(' forma simultanea. Con el argumento ADC=T se realizara ademas una ', fill=TRUE)
		cat(' estimacion cuadratica. ', fill=TRUE)
		cat('', fill=TRUE)
		cat("sites.google.com/site/ullrtoolbox/07---analisis-multivariado/analisis-discriminante-fnc", fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		print(argumentos('analisis.discriminante.fnc'))
	return('help')
	}
	require(car, quietly =TRUE)

	if(grupo=='grupo'){
		datos=cambia.nombre.var.fnc(datos,'grupo','grupos',silente=TRUE)
		grupo='grupos'
	}

	if(grafica) { graf.cor=TRUE; map=TRUE; roc=FALSE}
	if(!grafica) { graf.cor=FALSE; map=FALSE; roc=FALSE}

	if(!silente) crea.cat.fnc('ANALISIS DISCRIMINANTE')
	if(!silente) cat('  Argumentos: ',fill=TRUE)
	if(!silente)
	print(c(paso.a.paso=paso.a.paso,VC=VC,grafica=grafica,ordenado=ordenado,
		roc=roc,graf.cor=graf.cor,map=map,ADC=ADC))

	dat=datos

	# SI HAY O NO VARIABLES DEFINIDAS POR EL USUARIO
	if(is.na(variables[1])) { variables=names(dat); dat=dat[,variables] }
	if(is.numeric(variables)){
		nombres=names(dat)
		variables=nombres[variables]
	}

	# PONE EL FACTOR GRUPO COMO PRIMERA VARIABLE DEL SISTEMA
	col.gr=match(grupo,variables)
	if(is.na(col.gr)) {
		variables=c(grupo,variables)
		col.gr=match(grupo,variables)
	}
	dat=dat[,variables]
	if(!is.factor(dat[,grupo])) dat[,grupo]=as.factor(dat[,grupo])
	if(col.gr != 1){
 		dat=cbind(dat[,col.gr],dat[,-col.gr])
		variables=variables[-col.gr]
 		dat=data.frame(dat)
 		names(dat)=c(grupo,variables)
	}else{
	  	variables=variables[-1]
	}

	#--------------------------------------------------------
	# CHECK SI HAY FACTORES
	es.factor=logical( )
	check=data.frame(dat[,variables])
	names(check)=variables
	for (i in 1:dim(check)[2])
		 es.factor[i]=is.factor(check[,i])
	if(sum(es.factor)!=0){
		var.fac=variables[es.factor]
		cat('',fill=TRUE)
		cat('*** Hay variables categoricas en el modelo, utiliza la regresion ***',fill=TRUE)
		cat('*** logistica o recodifica la variable a su version dummy (0,1)  ***',fill=TRUE)
		cat('*** mediante la funcion: recodifica.a.dummy.fnc                  ***',fill=TRUE)
		cat('*** La variable ',var.fac,' es categorica.', fill=TRUE)
		cat('',fill=TRUE)
		stop( )
	}
	# -------------------------------------------------------
	if(!is.factor(dat[,grupo])) dat[,grupo]=factor[dat,grupo]
	n.group=nlevels(dat[,grupo])
	modelo=paste(grupo,' ~ .',sep='')
	metodo='todas las variables'

	# CHECK SI HAY VARIABLES COMPLETAMENTE MISS
	observ=dim(dat)[1]
	chivato=apply(is.na(dat),2,sum)
	indice= chivato==observ
	if(sum(indice)!=0){
	  var.NA=variables[indice[-1]]
	  cat('',fill=TRUE)
	  if(length(var.NA)==1){
	    cat('*** Error. La variable:',var.NA,' no contiene valores. ***',fill=TRUE)
	    cat('*** Eliminala del argumento variables.                    ',fill=TRUE)
	  }else{
	    cat('*** Error. Las variables:',var.NA,' no contienen valores. ***',fill=TRUE)
	    cat('*** Eliminalas del argumento variables.                    ',fill=TRUE)
	  }
	  cat('',fill=TRUE)
	stop( )
	}
	# FIN SI HAY VARIABLES COMPLETAMENTE MISS

	# PASO A PASO
	if(paso.a.paso){
		require('klaR', quietly = TRUE)
		gw_obj = try(greedy.wilks(eval(parse(text=modelo)),
				data = dat, niveau = 0.1), silent=TRUE)

		if(class(gw_obj)=='try-error'){
			cat('',fill=TRUE)
			cat('*** Error. Ha habido un problema con la estimacion paso a paso ***',fill=TRUE)
			cat('*** incluye el argumento paso.a.paso=F.                        ***',fill=TRUE)
			cat('',fill=TRUE)
			print(gw_obj)
		stop( )
		}

		variables=as.character(gw_obj$results[,1])
		if(length(variables)==0){
		  	cat('*** Ninguna variable ha resultado significativa en el procedimiento paso a paso ***',fill=TRUE)
			stop( )
		}
		dat=na.omit(dat[,c(grupo,variables)])
		metodo='paso a paso'
	} # FIN PASO A PASO

	dat=na.omit(dat)

	# HOMOGENEIDAD DE VARIANZAS Y COVARIANZAS
	if(length(variables) > 1){
		box.test=BoxMTest(dat[,-1],dat[,grupo])
	}else{
		box.test=leveneTest(dat[,variables],as.factor(dat[,grupo]))
	}

	eso=lda(eval(parse(text=modelo)),prior=rep(1/n.group,n.group),
			data=dat)

	# SI VALIDACION CRUZADA (JACKNIFE)'
	if(!is.na(VC[1])){
		vcruzada=lda(eval(parse(text=modelo)),prior=rep(1/n.group,n.group),
			CV=TRUE,data=dat)
		tabla.cv=table(dat[,grupo],vcruzada$class)
		nombre.tabla=dimnames(tabla.cv)
		names(nombre.tabla)=c('Observado','Predicho')
		dimnames(tabla.cv)=nombre.tabla
		tabla.pr.cv=prop.table(tabla.cv,1)
	} # FIN DE VALIDACION CRUZADA

	# SI N.VARIABLES ES 1 O NO
	if(length(variables)==1){
	  nf=1
	  mod=paste(variables,' ~ ',grupo,sep='')
	  anvar=with(dat, aov(eval(parse(text=mod))))
	  res=unlist(summary(anvar))
	  SCT = res['Sum Sq1']+res['Sum Sq2']
	  lwilks=as.numeric(1-(res['Sum Sq1']/SCT))
	}else{
	  if(grupo=='grupo'){
	  	mano=manova(as.matrix(dat[,variables]) ~ grupo,
			data=dat)
	  }else{
	  	mano=manova(as.matrix(dat[,variables]) ~ eval(parse(text=grupo)),
			data=dat)
	  }
	  anvar=summary(mano,test='Wilks')
	  nf=min(n.group-1,length(variables))
	  ev=(anvar$Eigenvalues)[1:nf]
	  lwilks=1/(1+ev)
	  canonica=sqrt(ev/(1+ev))
	  si.borra=0;	dif_=0
	  if(length(variables) > 2){
	  	for(i in 1:length(variables)){
			que.var=variables[-i]
			dat_=dat[,c(grupo,que.var)]
	  		if(grupo=='grupo'){
				mano_=summary(manova(as.matrix(dat_[,que.var]) ~ grupo,
					data=dat_))
	  		}else{
				mano_=summary(manova(as.matrix(dat_[,que.var]) ~ eval(parse(text=grupo)),
					data=dat_))
			}
	  		ev_=(mano_$Eigenvalues)[1:nf]
			lwilks_=1/(1+ev_)
			dif_=rbind(dif_,lwilks_-lwilks)
			si.borra=rbind(si.borra,lwilks_)
	  	}
	  	si.borra=round(si.borra[-1,],3)
	  	dif_=round(dif_[-1,],3)
	  	si.borra=cbind(si.borra,dif_)
	  	row.names(si.borra)=variables
	  	colnames(si.borra)=
	  	c(paste('W.f',1:nf,sep=''),paste('dif.Wf',1:nf,sep=''))
	}else{
		si.borra=NA
	}
	} # FIN SI N VARIABLES


	tabla=table(dat[,grupo],predict(eso)$class);
	nombre.tabla=dimnames(tabla)
	names(nombre.tabla)=c('Observado','Predicho')
	dimnames(tabla)=nombre.tabla
	lista=list(tabla=tabla, prop.tabla=prop.table(tabla,1))

	# Creamos resumenes
	var.pred=predict(eso); names(var.pred)
  	mod.pred=data.frame(gr.pred=var.pred[[1]],
		p.gr=round(var.pred[[2]],4),fd=var.pred[[3]])
	completo=cbind(dat,mod.pred)
 	coef.no.tipicos=round(eso$scaling,4)
 	medias=descriptivos.fnc(data.frame(dat[,-1]), silente=TRUE)[,2]
	b0=-(apply(coef.no.tipicos*medias,2,sum))
	
	col.grupo=match(grupo,names(dat))
	fun=data.frame(var.pred[[3]])
	fase1=cbind(fun,dat)
	col.gr=match(grupo,names(fase1))
	fase2=cbind(fun,dat[,grupo])
	names(fase2)=c(names(fun),grupo)

	#-----------------------------------------------------------------------------------
	# GRAFICA DE FUNCIONES. Si nf > 1
	n.fun=dim(fun)[2]
	if(n.fun >=2){
	    nombres.f=names(fun)
	    agrupa=cbind(fun,dat[,grupo])
	    names(agrupa)=c(nombres.f,grupo)
	    if(n.fun==2) colu=1:2; if(n.fun >=3) colu=list(1:2,c(1,3),c(2,3))
	    if(n.fun==2) grafica.xy.fnc(agrupa, variables=nombres.f[colu], que.factor=grupo)
	    if(n.fun >=3) {
	      par(mfrow=c(2,2))
	      print(grafica.xy.fnc(agrupa, variables=nombres.f[colu[[1]]], que.factor=grupo))
	      print(grafica.xy.fnc(agrupa, variables=nombres.f[colu[[2]]], que.factor=grupo))
	      print(grafica.xy.fnc(agrupa, variables=nombres.f[colu[[3]]], que.factor=grupo))
	      par(mfrow=c(1,1))
	    }
	   if(grafica) X11( )
	    rm(agrupa)
	}  # FIN DE GRAFICA DE FUNCIONES
	#-----------------------------------------------------------------------------------

	# CENTROIDES
	centr=list( )
	for(i in 1:nf){
	  if(grupo=='grupo'){
	  	centr[[i]]=with(fase2, tapply(fase2[,i], list(dat[,'grupo']),mean))
	  }else{
	  	centr[[i]]=with(fase2, tapply(fase2[,i], list(dat[,grupo]),mean))
	  }
	}
	centr=do.call(rbind,centr)
	centroides=t(centr)
	colnames(centroides)=names(fun)

    #---------------------------------------------------------------------------------------
    # PREPARA SALIDA SI UNA O VARIAS VARIABLES DISCRIMIANTES
    if(length(variables)!=1){
	# AVAR L-WILKS POR FUNCION
	canonica=sqrt(ev/(1+ev))
	res.wilk=0
	for(i in 1:nf){
		av1=summary(aov(fase1[,i]~ fase1[,col.gr],dat=fase1))
		F=unlist(av1)['F value1']
		df.1=unlist(av1)['Df1']; df.2=unlist(av1)['Df2'];
		p.val=round(unlist(av1)['Pr(>F)1'],6)
		res.wilk=rbind(res.wilk,c(Autovalor=ev[i],Canonica=canonica[i],
			Wilks=lwilks[i],F,df.1,df.2,p.val))
	}
	res.wilk=res.wilk[-1,]
	res.wilk=matrix(res.wilk,nrow=nf)
	row.names(res.wilk)=paste('func.',1:nf,sep='')
	colnames(res.wilk)=c('Autovalor','Canonica','Wilks','F.val','df.n','df.d','p.val')

	# CALCULAMOS COEFICINTES TIPICOS y estructura
 	gl.error=sum(table(dat[,grupo]))-n.group
 	coef.tipicos=coef.no.tipicos*sqrt(diag(anvar$SS[[2]]/gl.error))
	coef.estructura= cov2cor(anvar$SS[[2]]*(1/gl.error))%*%coef.tipicos

	lista=list(modelo=eso,metodo.entrada=metodo,
		variables.seleccionadas=variables,
		medias=round(t(eso$means),3),
		manova.omnibus= summary(mano),
		funciones=res.wilk,
		Wilks.si.var.es.eliminada=si.borra,
		homogeneidad.varianzas.covarianzas=box.test,
		centroides=centroides,
		coef.no.tipicos=rbind(coef.no.tipicos,b0),
		coef.tipicos=coef.tipicos,
		coef.estructura=coef.estructura,
		clasificacion=tabla,
		prop.tabla=prop.table(tabla,1))

    # SI SOLO UNA VARIABLE DISCRIMINANTE
    }else{
	  coef.tipicos=data.frame(LD1=1)
	  row.names(coef.tipicos)=variables
	  coef.estructura=data.frame(LD1=1)
	  row.names(coef.estructura)=variables
	  lista=list(modelo=eso,metodo.entrada=metodo,
		variables.seleccionadas=variables,
		medias=round(t(eso$means),3),
		anova.omnibus= summary(anvar),
		funciones=c(wilks=lwilks),
		homogeneidad.varianzas.covarianzas=box.test,
		centroides=centroides,
		coef.no.tipicos=rbind(coef.no.tipicos,b0),
		coef.tipicos=coef.tipicos,
		coef.estructura=coef.estructura,
		clasificacion=tabla,
		prop.tabla=prop.table(tabla,1))
    } # FIN SI SOLO UNA VAR. DISCRIMINANTE
    #---------------------------------------------------------------------------------------

    #---------------------------------------------------------------------------------------
	# SI VALIDACION CRUZADA
	if( !is.na(VC[1]) & length(variables)!=1 ){
	  lista=list(modelo=eso,metodo.entrada=metodo,
		variables.seleccionadas=variables,
		medias=round(t(eso$means),3),
		manova.omnibus= summary(mano),
		funciones=res.wilk,
		Wilks.si.var.es.eliminada=si.borra,
		homogeneidad.varianzas.covarianzas=box.test,
		centroides=centroides,
		coef.no.tipicos=rbind(coef.no.tipicos,b0),
		coef.tipicos=coef.tipicos,
		coef.estructura=coef.estructura,
		clasificacion=tabla,
		prop.tabla=prop.table(tabla,1),
		validacion.cruzada.jacknife=tabla.cv,
		prop.validacion.cruzada=tabla.pr.cv)
	}
	if( !is.na(VC[1]) & length(variables)==1 ){
	  lista=list(modelo=eso,metodo.entrada=metodo,
		variables.seleccionadas=variables,
		medias=round(t(eso$means),3),
		anova.omnibus= summary(anvar),
		funciones=c(wilks=lwilks),
		homogeneidad.varianzas.covarianzas=box.test,
		centroides=centroides,
		coef.no.tipicos=rbind(coef.no.tipicos,b0),
		coef.tipicos=coef.tipicos,
		coef.estructura=coef.estructura,
		clasificacion=tabla,
		prop.tabla=prop.table(tabla,1) )
	} # FIN SI VALIDACION CRUZADA
        #---------------------------------------------------------------------------------------

	if(grafica){
	  require(lattice, quietly=TRUE)
	  X11()
	  for(i in 1:dim(fun)[2]){
	  X11()
	  print(densityplot(~ fun[,i], group=gr.pred, data=mod.pred,
		xlab=names(fun)[i],
		main=paste('Histograma de la funcion ',i,' por grupo',sep=''),
	    key = simpleKey(text = levels(mod.pred$gr.pred),
	    space = "right", points = TRUE)))}
	 }
	 if(roc){
	    if(n.group > 2) {
		 cat('*** La curva ROC requiere solo dos grupos ***',fill=TRUE)
		}else{
	     require('ROCR', quietly=TRUE)
	     if(!is.factor(dat[,grupo]))
		  dat[,grupo]=as.factor(dat[,grupo])
		  niv.grupo=levels(dat[,grupo])
		  diag.0=as.numeric(completo[,grupo])-1
		  diag.1=recode(diag.0," 0=1; 1=0")
		  pred.0=prediction(mod.pred[,2],diag.0)
		  pred.1=prediction(mod.pred[,3],diag.1)
		  perf.0 = performance(pred.0,"tpr","fpr")
		  perf.1 = performance(pred.1,"tpr","fpr")
		  X11( )
		  par(mfrow=c(1,2))
		  plot(perf.0,colorize=TRUE,main=niv.grupo[2],
		   cex.label=0.5,print.cutoffs.at=seq(0,1,by=0.1))
 		  plot(perf.1,colorize=TRUE,main=niv.grupo[1],
		   cex.label=0.5,print.cutoffs.at=seq(0,1,by=0.1))
		 par(mfrow=c(1,1))
	    }
	  } # CIERRA ROC
	  if(graf.cor & length(variables) > 1){
	    #X11()
	    grafica.correlacion.fnc(dat,col.grupo,size.font=0.6)
	  }
	  if(map & length(variables) > 1) {
	  X11( )
	  require(klaR, quietly=TRUE)
	  n.var=length(variables)
	  n.comp=ceiling((n.var*n.var-1)/2)
	  alma=matrix(NA,n.var,2)
	  for (i in 1:n.var){
	    alma[i,2]=(n.comp-(i*i))
	    alma[i,1]=i
	  }
	  n.col=alma[alma[,2]==min(abs(alma[,2])),][1]
	  partimat(eval(parse(text=modelo)),prior=rep(1/n.group,n.group),
		data=dat,method='lda',plot.matrix=TRUE)
	  }
	#try(detach(package:car),silent=TRUE)
	try(detach(package:klaR),silent=TRUE)
	

	# SI AD CUADRATICO
	if(ADC){
	    eso.qda=try(qda(eval(parse(text=modelo)),prior=rep(1/n.group,n.group),
			data=dat),silent=TRUE)

	    if(class(eso.qda)!='try-error'){
	    	qda.pred = predict(eso.qda)$class
	    	completo.qda=cbind(dat,qda.pred)
	    	head(completo.qda)
	    	qda.pred=frecuencias.fnc(completo.qda, paste(grupo,':','qda.pred',sep=''),
			prop=TRUE, silente=TRUE)[-4]
	    	qda.list=list( )
	    	n.list=length(lista)
	    	nombres=names(lista)
	    	for (i in 1:(n.list+2)){
			if(i <= n.list) qda.list[[i]] = lista[[i]]
			if(i==n.list+1) qda.list[[i]]= eso.qda
			if(i==n.list+2) qda.list[[i]]= qda.pred
	    	}
	    	names(qda.list)=c(nombres,'modelo.adc','adc.pred')
	    	if(!silente) print(qda.list)
		return(qda.list)
	    }else{
		cat('',fill=TRUE)
		print('*** No ha sido posible estimar el modelo cuadratico ***',fill=TRUE)
		cat('',fill=TRUE)
		return(lista)
	    }
	}else{
	    	if(!silente) print(lista)
	    	if(!silente & latex) latex.fnc(lista[-c(1:5)])
	return(lista)
	}
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# Extrae las puntuaciones discriminantes de un modelo discriminante estimado
# previamente y las anade a la base de datos del modelo. Asimismo devuelve
# el grupo pronosticado para cada registro. Si se utilizace una base de datos
# diferente, haria la prediccion para esta nueva base de datos
#---------------------------------------------------------------------------
 puntuacion.discriminante.fnc=function(datos,modelo){
	dat=datos
	nombres=names(dat)
	funci=paste('LD',1:30,sep='')
	grpre=paste('grupo.predicho',1:30,sep='')
	chivato1=funci[funci %in% nombres]
	chivato2=grpre[grpre %in% nombres]
 	mod=modelo$modelo
	pd=suppressWarnings(data.frame(predict(mod,dat)$x))
	ndim=dim(pd)[2]
	clase=suppressWarnings(data.frame(predict(mod,dat)$class))
	names(clase)='grupo.predicho1'
	if(!is.na(chivato1[1]))	{
		que.var=chivato1
		que.var=que.var[length(que.var)]
		n=nchar(que.var)
		que.var=as.numeric(substr(que.var,3,n))
		names(pd)=paste('LD',(que.var+1):(que.var+ndim),sep='')
	}
	if(!is.na(chivato2[1]))	{
		que.var=chivato2
		que.var=que.var[length(que.var)]
		n=nchar(que.var)
		que.var=as.numeric(substr(que.var,15,n))
		names(clase)=paste('grupo.predicho',(que.var+1),sep='')
	}
	dat_=cbind(dat,pd,clase)
	cat('',fill=TRUE)
	cat('*** Esta es la cabecera de tus nuevos datos. Se han incluido las puntuaciones discriminantes ***',fill=TRUE)
	cat('',fill=TRUE)
	print(head(dat_))
 return(dat_)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# correlacion.fnc(datos)
#---------------------------------------------------------------------------
 correlacion.fnc=function(datos=NA, variables=NA, tipo=NA, covarianza=FALSE, silente=FALSE,
		contraste=NA, que.factor=NA, caso.completo=TRUE, parcial=FALSE,
	        sparcial=FALSE, control=NA){

	if(class(try(is.na(datos)))=='logical') {
		cat('',fill=TRUE)
		crea.cat.fnc('correlacion.fnc')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat('*** Pearson Listwise ***', fill=TRUE)
		cat("  correlacion.fnc(iris, caso.completo=T)    ",fill=TRUE)
		cat("  correlacion.fnc(iris, variables=1:4)    ",fill=TRUE)
		cat("  correla = correlacion.fnc(iris, variables=1:4, silente=T)    ",fill=TRUE)
		cat("  correlacion.fnc(iris, variables=1:4, que.factor='Species')    ",fill=TRUE)
		cat('', fill=TRUE)
		cat('*** Covarianza ***', fill=TRUE)
		cat("  correlacion.fnc(iris, variables=1:4, covarianza=T)    ",fill=TRUE)
		cat('', fill=TRUE)
		cat('*** Policorica ***', fill=TRUE)
		cat("  correlacion.fnc(OBrienKaiser, variables=3:6,tipo='policorica')    ",fill=TRUE)
		cat('', fill=TRUE)
		cat('*** Parcial y Semiparcial ***', fill=TRUE)
		cat("  correlacion.fnc(OBrienKaiser, variables=3:7,    ",fill=TRUE)
		cat("  		parcial=T, control=c(8,10,12) )    ",fill=TRUE) 
		cat('', fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Estima la matriz de correlaciones o covarianza de Pearson, Spearman,   ', fill=TRUE)
		cat(' kendall, parcial, semiparcial y distancias de las variables cuantitativas', fill=TRUE)
		cat(' definidas por el usuario. Si se desea utilizar la matriz de correlaciones ', fill=TRUE)
		cat(' estimada como input de otro procedimiento (analisis factorial) debera', fill=TRUE)
		cat(' incluirse el argumento silente=T', fill=TRUE)
		cat('', fill=TRUrE)
		cat(" sites.google.com/site/ullrtoolbox/estadisticos-descriptivos/correlacion-fnc", fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		print(argumentos('correlacion.fnc'))
	return('help')
	}

	dat=datos
	nombres=names(datos)
	check.que.factor(que.factor)
	
	# DETERMINA TIPO DE CORRELACION SOLICITADA
	if(is.na(tipo[1])) tipo='pearson'
	if(tipo=='ke') tipo='kendall'
	if(tipo=='sp') tipo='spearman'
	# FIN TIPO DE CORRELACION
	
	# CHECK TIPO
	chivato.tipo=tipo %in% c('pearson','kendall','spearman','policorica','tetracorica')
	if(!chivato.tipo){
		cat('',fill=TRUE)
		cat('*** Error. Has indicado un tipo de correlaciones no reconocido.  ***',fill=TRUE)
		cat('*** Puedes indicar como tipo en ese argumento los siguientes:    ***',fill=TRUE)
		cat("*** pearson,kendall,spearman,policorica y tetracorica.           ***",fill=TRUE)
		cat("*** Ej. correlacion.fnc(datos, variables=2:12, tipo='kendall')   ***",fill=TRUE)
		cat('',fill=TRUE)
		stop( )
	} # FIN CHECK	
	
	# CHECK SI NA VARIABLES Y QUE.FACTOR
	if(is.na(variables[1]) & !is.na(que.factor[1])) {
		cat('',fill=TRUE)
		cat('*** Error. Si deseas las correlaciones o covarianzas por cada nivel      ***',fill=TRUE)
		cat('*** de un factor, debes determinar el nombre o numero de las variables   ***',fill=TRUE)
		cat('*** cuantitativas de las que deseas la matriz en el argumento variables. ***',fill=TRUE)
		cat("*** Ej. variables=1:12 o variables=c('peso','longitud','forma')          ***",fill=TRUE)
 	 stop( )
	} # FIN CHECK

	# EXISTE QUE.FACTOR
	if(!is.na(que.factor)){
		# EXISTEN LAS VARIABLES
		check=existe.variable.fnc(datos,que.factor)
		if(check$cc !=0) {
			cat('',fill=TRUE)
			cat('*** Error. No existe la variable o variables:',check$faltan,fill=TRUE)
			cat('*** en la base de datos incluida. Revisa el argumento que.factor',fill=TRUE)
		stop( )
		}
	}
	# FIN EXISTE QUE.FACTOR

	if(is.na(variables[1]) & is.na(que.factor[1])) {
		variables=names(dat)
		dat__=dat
	}

	# EXISTEN VARIABLES
	if(!is.na(variables[1])){
		if(is.numeric(variables)) variables=nombres[variables]
		# EXISTEN LAS VARIABLES
		check=existe.variable.fnc(datos,variables)
		if(check$cc !=0) {
			cat('',fill=TRUE)
			cat('*** Error. No existe la variable o variables:',check$faltan,fill=TRUE)
			cat('*** en la base de datos incluida. Revisa el argumento variables',fill=TRUE)
		stop( )
		}
	}
	# FIN EXISTEN VARIABLES

	if(!is.na(variables[1]) & is.na(que.factor[1])) {
		dat=dat[,variables];	dat__=dat
	}
	if(!is.na(variables[1]) & !is.na(que.factor[1])){
		nombres=names(dat)
		if(is.numeric(variables)) variables=nombres[variables]
 		dat=dat[,c(que.factor,variables)]
		dat__=dat[,variables]
	} # FIN SI TODA LA DATA O NO

	hay.factores=dime.si.hay.factores.fnc(dat__)
	if(length(hay.factores[[2]]) > 0){
		cat('',fill=TRUE)
		cat('*** Error. Hay factores entre las variables implicadas. No puede calcularse ***',fill=TRUE)
		cat('*** la matriz solicitada si todas las variables a analizar no son continuas ***',fill=TRUE)
		cat('*** Estas son tus variables no cuantitativas erroneamente incluidas:        ***',fill=TRUE)
		cat('',fill=TRUE)
		cat('*** factores: ',hay.factores[[2]],fill=TRUE)
		cat('*** Elimina estas columnas del argumento variables: ',hay.factores[[1]],fill=TRUE)
	 stop( )
	}
	if(!silente){
		cat('',fill=TRUE)
		cat('*** Si deseas utilizar la matriz de correlaciones como imput para otra ***',fill=TRUE)
		cat('*** aplicacion como analisis.factorial.fnc, debes incluir el argumento ***',fill=TRUE)
		cat('*** silente=T y asignar la salida al objeto que desees.                ***',fill=TRUE)
		cat('*** Ej:                                                                ***',fill=TRUE)
		cat('*** correla=correlacion.fnc(datos, variables=2:10, silente=T)          ***',fill=TRUE)
		cat('*** analisis.factorial.fnc(matcor=correla, n.factores=2)               ***',fill=TRUE)
		cat('',fill=TRUE)
	}

	if(length(variables)==2) caso.completo=T

	# TRANSFORMA LA DATA EN UNA LISTA
 	if(!is.na(que.factor[1])){
		dat=split(dat,dat[,que.factor])
		nombres.qf=names(dat)
	}else{
		dat=list(dat)
		titulo=''
	}

	# SI QUE.FACTOR
	lista.out=list( )
	for(j in 1:length(dat)){
	  dat_=dat[[j]]

	  # SI QUE.FACTOR ELIMINA EL FACTOR ANTES DEL CALCULO DE COR
	  if(!is.na(que.factor[1])) {
		nombres=names(dat_); que.col=match(que.factor,nombres)
		dat_=dat_[,-que.col]
		titulo=paste(que.factor,' en nivel: ',nombres.qf[j],sep='')
	  } # FIN ELIMINA QUE.FACTOR DE LA DATA_

	  # FIN SI TODA LA DATA
	  if(caso.completo){
	    dat_=na.omit(dat_)
	    n.suj=dim(dat_)[1]
	    que.datos='complete.obs'
	    label=' Listwise: Caso Completo'
	  }else{
	    dat_=dat_
	    que.datos='pairwise'
	    n.suj=matrix.n.pairwise.fnc(dat_)
	    label=' Pairwise: Elimina por pares'
	  }
	
	  # SI CORRELACION PARCIAL O SEMIPARCIAL
	  if(parcial | sparcial){
	     cor.mat=correlacion.parcial.fnc(datos, variables, parcial=parcial, 
				sparcial=sparcial, control=control,silente=silente)
	     if(!silente){
			print(cor.mat)
	        return( )
	     }else{
	        return(cor.mat)
	     }
	  }

	  # SI COVARIANZA
	  if(!covarianza){
		if(tipo != 'policorica' & tipo !='tetracorica'){
			correla=round(cor(dat_,method=tipo,use=que.datos),3)
			corre='correlacion'
		}
		#suppressWarnings(require(polycor, quietly=TRUE))
		require(psych, quietly=TRUE)
		if(tipo=='policorica'){
			correla=suppressWarnings(try(polychoric(dat_, smooth=TRUE, 
					global=TRUE, polycor=T,
	 				ML = FALSE, std.err=FALSE), silent=TRUE))
			if(class(correla)[1]=='try-error'){
				cat('',fill=TRUE)
				cat('*** Error. No ha sido posible estimar la matriz policorica ***',fill=TRUE)
				cat('',fill=TRUE)
				print(correla)
				stop( )
			}
			correla=correla[[1]]
		}
		if(tipo=='tetracorica'){
			correla=suppressWarnings(try(tetrachoric(dat_,correct=TRUE,
				smooth=TRUE,global=TRUE), silent=TRUE))
			if(class(correla)[1]=='try-error'){
				cat('',fill=TRUE)
				cat('*** Error. No ha sido posible estimar la matriz tetracorica ***',fill=TRUE)
				cat('',fill=TRUE)
				print(correla)
				stop( )
			}
			correla=correla[[1]]
		}
	  }else{
		correla=round(cov(dat_,method=tipo,use=que.datos),3)
		corre='covarianza'
	  } # FIN SI COVARIANZA
	  lista.out[[j]]=correla

	  tipo1='pearson'
	  # SI HAY O NO CONTRASTES
	  if(!is.na(contraste[1])){
		if(tipo=='policorica') {tipo1='policorica'; tipo='pearson'}
		if(tipo=='tetracorica') {tipo1='tetracorica'; tipo='pearson'}
		dat_=na.omit(dat_)
		fac.intra=list(fac=names(dat_))
 		apilados=apila.los.datos.fnc(dat_, fac.intra=fac.intra,
			col.empieza.mr=1,silente=TRUE)
		pares=tot.pares.fnc(matrix(1:length(names(dat_)),1,length(names(dat_))))
		etiquetas=etiqueta.fnc(fac.intra[[1]],pares)
		alma=list( )
		if(length(names(dat_))==2) dim(pares)=c(1,2)
		# PARA TODOS LOS PARES DE VARIABLES
		for(i in 1:dim(pares)[1]){
			cont=try( cor.test(as.numeric(dat_[,pares[i,1]]),
				as.numeric(dat_[,pares[i,2]]), method = tipo), silent=TRUE)
			r=cont$estimate; 	t=cont$statistic;
			gl=cont$parameter;	p=cont$p.value
			alma[[i]]=round(c(r,t,gl,p.val=p),4)
 		} # FIN DE PARES
		alma=data.frame(do.call(rbind,alma))
		alma$variables=etiquetas
		if(tipo=='pearson') {
			alma=alma[,c(5,1:4)]
			crea.cat.fnc(paste(titulo,' Contraste Ho: rxy = 0',sep=' '))
		}
		if(tipo=='spearman'){
			alma=alma[,c(4,1:3)]
			crea.cat.fnc(paste(titulo,' Contraste Ho: rho.xy = 0',sep=' '))
		}
		if(tipo=='kendall'){
			alma=alma[,c(4,1:3)]
			crea.cat.fnc(paste(titulo,' Contraste Ho: tau.xy = 0',sep=' '))
		}
		if(tipo1=='policorica'){
			alma=alma[,c(4,1:3)]
			crea.cat.fnc(paste(titulo,' Contraste Ho: rxy(Pearson, NO POLICORICA) = 0',sep=' '))
		}
		if(tipo1=='tetracorica'){
			alma=alma[,c(4,1:3)]
			crea.cat.fnc(paste(titulo,' Contraste Ho: rxy(Pearson, NO TETRACORICA) = 0',sep=' '))
		}
		
		print(alma)
		cat('',fill=TRUE)
		cat('*** El contraste de hipotesis se ha hecho desde la matriz listwise ***',fill=TRUE)
		cat('',fill=TRUE)
	  } # FIN DE SI CONTRASTES

	  if(tipo1=='policorica') tipo='policorica'
	  if(tipo1=='tetracorica') tipo='tetracorica'

	  if(!silente & !covarianza){
		titulo2=paste('  MATRIZ DE CORRELACION:',tipo,sep=' ')
		titulo2=paste(titulo,titulo2,sep='')
		crea.cat.fnc(titulo2)
	  }
	  if(!silente & covarianza){
		titulo2=paste('MATRIZ DE VAR. COVARIANZAS DE:',tipo,sep=' ')
		titulo2=paste(titulo,titulo2,sep='')
		crea.cat.fnc(titulo2)
	  }
	  if(caso.completo){
	      lista=list(n.suj.Listwise=n.suj,correlacion=correla)
	  }else{
	      lista=list(n.suj.Pairwise=n.suj,correlacion=correla)
	  }
	  if(!silente) print(lista) 
	} # FIN QUE.FACTOR
    try(detach(package:ltm),silent=TRUE) #detach de nlme si estuviese cargada
	try(detach(package:polycor),silent=TRUE)
	if(j > 1){
		names(lista.out)=levels(factor(datos[,que.factor]))
	}else{
		lista.out=lista.out[[1]]
	}
	if(silente) return(lista.out) 
 } # FIN RUTINA
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# Calcula la matriz de correlacion de p variables controladas (parcial o
# semiparcial) por otras variables definidas en el argumento control
#---------------------------------------------------------------------------
 correlacion.parcial.fnc=function(datos, variables, parcial=FALSE, sparcial=FALSE, 
	control=NA, silente=NA){

    	require(ppcor, quietly =TRUE)
	nombres=names(datos)
	if(is.na(control[1])){
	  cat('',fill=TRUE)
	  cat('*** Error. El argumento control esta vacio. Debes indicar al menos una ',fill=TRUE)
	  cat('*** variable por la que deseas controlar (parcial o semiparcial) la matriz ',fill=TRUE)
	  cat('*** de correlaciones de las variables incluidas en el argumento variables',fill=TRUE)
	  cat('',fill=TRUE)
	  stop( )
	}

	n.parcial=length(control)
	if(is.numeric(control)){
		control=nombres[control]
	}
	chivato=match(control, variables)
	if(sum(is.na(chivato))!=n.parcial){
		cat('',fill=TRUE)
		cat('*** Error. El argumento variables no debe contener las variables incluidas',fill=TRUE)
		cat('*** en el argumento control.',fill=TRUE)
		cat('',fill=TRUE)
		stop( )
	}
	if(is.na(silente[1]) & parcial){
	    titulo='  MATRIZ DE CORRELACION: Pearson Parcial'
		crea.cat.fnc(titulo)
	}
	if(is.na(silente[1]) & sparcial){
	    titulo='  MATRIZ DE CORRELACION: Pearson Semiparcial'
		crea.cat.fnc(titulo)
	}

	cat('',fill=TRUE)
	if(n.parcial==1){
	   if(parcial){
	      cat('***    Correlacion controlada (parcial) para la variable', control,fill=TRUE)
	   }
	   if(sparcial){
	      cat('***    Correlacion controlada (semiparcial) para la variable', control,fill=TRUE)
	   }
	}else{
	   if(parcial){
	      cat('***    Correlacion controlada (parcial) para las variable', control,fill=TRUE)
	   }
	   if(sparcial){
	      cat('***    Correlacion controlada (semiparcial) para las variable', control,fill=TRUE)
	   }
	}
	cat('',fill=TRUE)

	n.var=length(variables)
 	Xi=matrix(1:(n.var),1,n.var);
	pares=tot.pares.fnc(Xi)
	salida=list( )
	nombre.filas=list( )
	cor.mat=diag(1,n.var)
	for(i in 1:dim(pares)[1]){
		indice=as.numeric(pares[i,])
		que.par=datos[,indice]
		que.par=cbind(que.par,datos[,control])
		que.par=na.omit(que.par)
		if(parcial){
			res=pcor.test(que.par[,1],que.par[,2],que.par[,3:(2+n.parcial)])
		}
		if(sparcial){
			res=spcor.test(que.par[,1],que.par[,2],que.par[,3:(2+n.parcial)])
		}
		salida[[i]]=res
		paste(variables[indice[1]],variables[indice[2]],sep='-')
		nombre.filas[[i]]=paste(variables[indice[1]],variables[indice[2]],sep='-')
		cor.mat[indice[1],indice[2]]=res$estimate
	}
 	salida=data.frame(do.call(rbind,salida))
	nombre.filas=as.character(do.call(rbind,nombre.filas)[,1])
	row.names(salida)=nombre.filas
	if(is.na(silente[1])){print(salida); cat('',fill=TRUE)}
 	cor.mat=cor.mat+t(cor.mat)-diag(1,n.var)
 	colnames(cor.mat)=variables
 	rownames(cor.mat)=variables
	try(detach(package:ppcor),silent=TRUE)
 return(cor.mat)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# Estima el n por celdilla de una matriz pairwise.
#---------------------------------------------------------------------------
 matrix.n.pairwise.fnc=function(datos){
    variables=names(datos)
    pares=tot.pares.fnc(matrix(1:length(variables),ncol=length(variables)))
    pares2=pares[,c(2,1)]
    n=integer( )
    for(i in 1:(dim(pares)[1])) n[i]=dim(na.omit(datos[,pares[i,]]))[1]
    n=c(n,n)
    pares=rbind(pares,pares2)
    matriz=matrix(NA,length(variables),length(variables))
    for(i in 1:dim(pares)[1]) matriz[pares[i,1],pares[i,2]]=n[i]
    diag(matriz)=descriptivos.fnc(datos,silente=TRUE)$n
    dimnames(matriz)=list(names(datos),names(datos))
  return(matriz)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# grafica.correlacion.fnc(datos, vector.variables.excluidas, caso.completo=T)
#---------------------------------------------------------------------------
 grafica.correlacion.fnc=function(datos,excluidas=NA,size.font=1,correlaciones=FALSE,
		variables=NA, que.factor=NA, caso.completo=TRUE){
	require(psych, quietly = TRUE)
	check.que.factor(que.factor)

	# SI NO EXCLUIDAS Y NO VARIABLES
	if(is.na(excluidas[1]) & is.na(variables[1])){
		 datos_=datos
	}
	if(!is.na(excluidas[1]) & is.na(variables[1])){
		datos_=datos[,-excluidas]
	}
	if(is.na(excluidas[1]) & !is.na(variables[1])){
		datos_=datos[,variables]
	}

	# CHECK SI DATOS ES MATRIZ DE CORRELACIONES
	size=dim(datos_)
	if(size[1]==size[2]){
		if(sum(diag(as.matrix(datos_)))==size[2]) hay.cor=TRUE
	}else{
		hay.cor=FALSE
	}

	if(hay.cor){
		X11( )
		try(cor.plot(datos_, color = TRUE,zlim = c(-1, 1),
			cex.axis=size.font),silent=TRUE)
	return( )
	}

	# SI QUE.FACTOR O NO
	if(is.na(que.factor[1])){
		hay.factores=dime.si.hay.factores.fnc(datos_)
		# CHECK SI FACTORES
		if(length(hay.factores[[2]]) > 0){
			cat('',fill=TRUE)
			cat('*** Error. Hay factores entre las variables implicadas. No puede calcularse ***',fill=TRUE)
			cat('*** la matriz solicitada si todas las variables a analizar no son continuas ***',fill=TRUE)
			cat('*** Estas son tus variables no cuantitativas erroneamente incluidas:        ***',fill=TRUE)
			cat('',fill=TRUE)
			cat('*** factores: ',hay.factores[[2]],fill=TRUE)
			cat('*** Elimina estas columnas del argumento variables: ',hay.factores[[1]],fill=TRUE)
			cat('*** o incluye el argumento excluidas igual a los valores: ',hay.factores[[1]],fill=TRUE)
	 	stop( )
		}else{
	 	corre=correlacion.fnc(datos_,silente=TRUE,caso.completo=caso.completo)
		}
		X11( )
		suppressWarnings(try(cor.plot(corre, color = TRUE,zlim = c(-1, 1),
			cex.axis=size.font),silent=TRUE))
		if(correlaciones) return(corre)
	}else{
		nombres=names(datos_)
		col.fac=match(que.factor,nombres)
		hay.factores=dime.si.hay.factores.fnc(datos_[,-col.fac])
		# CHECK SI FACTORES
		if(length(hay.factores[[2]]) > 0){
			cat('',fill=TRUE)
			cat('*** Error. Hay factores entre las variables implicadas. No puede calcularse ***',fill=TRUE)
			cat('*** la matriz solicitada si todas las variables a analizar no son continuas ***',fill=TRUE)
			cat('*** Estas son tus variables no cuantitativas erroneamente incluidas:        ***',fill=TRUE)
			cat('',fill=TRUE)
			cat('*** factores: ',hay.factores[[2]],fill=TRUE)
			cat('*** Elimina estas columnas del argumento variables: ',hay.factores[[1]],fill=TRUE)
			cat('*** o incluye el argumento excluidas igual a los valores: ',hay.factores[[1]],fill=TRUE)
	 	stop( )
		}else{
			x.factor=split(datos_,datos_[,que.factor])
			for(i in 1:length(x.factor)){
				x=x.factor[[i]]
				X11( )
	 			corre=correlacion.fnc(x[,-col.fac],silente=TRUE)
				nivel=as.character(unique(x[,que.factor]))
				suppressWarnings(try(cor.plot(corre, main=paste(que.factor,' = ',nivel,sep=''),
					color = TRUE,zlim = c(-1, 1),
					cex.axis=size.font),silent=TRUE))
			}
		} # CIERRE SI NO HAY FACTORES ENTRE LAS VARIABLES
	} # CIERRE SI QUE.FACTOR
#	try(detach(package:psych),silent=TRUE)
 }
#---------------------------------------------------------------------------

# -------------------------------------------------------------------------------
#
# -------------------------------------------------------------------------------
 Anova.fnc=function(datos=NA,fac.intra=NA, col.empieza.mr=NA,
		fac.inter=NA, vd=NA, tipo=3,covariante=NA,grafica=TRUE,lim=NA, ylim=NA, to.pdf=FALSE,
		size.font=1, apaisado=FALSE,silente=NA, cova.x.vi=NA, nombre=NA, poshoc=NA,
		contrastes=NA, etiqueta=NA, color=TRUE, latex=FALSE){
	# lim es un coeficiente corrector que permite definir el limite superior de
	# la grafica de intervalos de confianza del anova. lim es el factir multiplicador
	# del error tipico asociado a las medias que van a plotearse. Incrementando lim
	# podemos conseguir el hueco que fuera necesario para ubicar la leyenda de la
	# grafica

	if(class(try(is.na(datos)))=='logical') {
		cat('',fill=TRUE)
		crea.cat.fnc('ANALISIS DE LA VARIANZA')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat('# Anova multifactorial intergrupo #					',fill=TRUE)
		cat("  Anova.fnc(datos, vd='tr', fac.inter=c('genero','zona'))			",fill=TRUE)
		cat('# Las graficas seran en tonos de grises (color=F) #			',fill=TRUE)
		cat("  Anova.fnc(datos, vd='tr', fac.inter=c('genero','zona'), color=F)		",fill=TRUE)
		cat('',fill=TRUE)
		cat('# Anova multifactorial completamente repetido 				',fill=TRUE)
		cat("  fac.intra=list(A=c('A1','A2'), B=c('B1','B2')) 				",fill=TRUE)
		cat("  Anova.fnc(datos, fac.intra=fac.intra, col.empieza.mr=5)			",fill=TRUE)
		cat('',fill=TRUE)
		cat('# Anova multifactorial split-plot #					',fill=TRUE)
		cat("  fac.intra=list(A=c('A1','A2'), B=c('B1','B2')) 				",fill=TRUE)
		cat("  fac.inter=c('genero','zona')						",fill=TRUE)
		cat('',fill=TRUE)
		cat("  Anova.fnc(datos, fac.inter=fac.inter, fac.intra=fac.intra)		",fill=TRUE)
		cat("  		col.empieza.mr=5)						",fill=TRUE)
		cat("  Anova.fnc(datos, fac.inter=fac.inter, fac.intra=fac.intra, tipo=2,	",fill=TRUE)
		cat("  		col.empieza.mr=5)						",fill=TRUE)
		cat("  Anova.fnc(datos, fac.inter=fac.inter, fac.intra=fac.intra, tipo=2,	",fill=TRUE)
		cat("  		col.empieza.mr=5, to.pdf=T, lim=10, poshoc='genero:zona:A')  	",fill=TRUE)
		cat('',fill=TRUE)
		cat('# ANCOVA con interaccion de covariante por factor#				',fill=TRUE)
		cat("  Anova.fnc(datos, fac.inter=fac.inter, fac.intra=fac.intra,		",fill=TRUE)
		cat("  		col.empieza.mr=5,covariante='edad' , cova.x.vi='gender')	",fill=TRUE)
		cat('',fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Lleva a cabo ANOVA multifactorial inter, intra y splip-plot con sumas	', fill=TRUE)
		cat(' de cuadrados tipo 2 y 3 asi como ANCOVA y estimacion de contrastes poshoc', fill=TRUE)
		cat(' de efectos simples, interacciones triples, ortogonales y de tendencia.', 	fill=TRUE)
		cat(' El argumento to.pdf=T envia a un archivo pdf las graficas de barras con', fill=TRUE)
		cat(' intervalos de confianza para cada uno de los efectos estimados.		', fill=TRUE)
		cat('',fill=TRUE)
		cat("          https://sites.google.com/site/ullrtoolbox/anova			", fill=TRUE)
		cat(' Estos son los argumentos de la funcion Anova.fnc:				', fill=TRUE)
		print(argumentos('Anova.fnc'))
		cat('---------------------------------------------------------------------', fill=TRUE)
	return('help')
	}
	crea.cat.fnc('ANALISIS DE LA VARIANZA')
	require(car, quietly = TRUE)

	if(latex){
		cat('',fill=TRUE)
		cat('*** Anova.fnc no admite por el momento el argumento latex ***',fill=TRUE)
		cat('',fill=TRUE)
	}		
	check.fac.intra.fnc(fac.intra)
	check.factores.fnc(fac.inter,fac.intra)
	check.niveles.fnc(datos, fac.inter=fac.inter, fac.intra=fac.intra)
	
	if(!is.na(fac.intra[1]) & is.na(col.empieza.mr[1]) & is.na(silente)){
		cat('',fill=TRUE)
		cat('*** Error. Dispones de medidas repetidas y no has incluido el argumento:',fill=TRUE)
		cat('*** col.empieza.mr donde indicas en que columna empiezan las medidas repetidas.',fill=TRUE)
		cat('*** Ej: col.empieza.mr=3 ',fill=TRUE)
		cat('',fill=TRUE)
		stop( )
	}

      if(!is.na(fac.inter[1])) {
		n.inter=length(fac.inter)
		ok.fac.inter=check.fac.inter(datos, fac.inter)
		if(length(ok.fac.inter)!=0)
			stop('*** No existe algun o algunos de los factores inter incluidos ***')
			
		# CHECK IF FACTORS ARE NUMERIC
		check.fac=dime.si.hay.factores.fnc(datos)$factores
		for(i in 1:length(fac.inter))
		  if(!(fac.inter[i] %in% check.fac)) datos[,fac.inter[i]]=as.factor(datos[,fac.inter[i]])
		# END OF CHECK IF FACTORS ARE NUMERIC	
					
	 }else{
		n.inter=0
	 }

      if(!is.na(fac.intra[1])) n.intra=length(names(fac.intra)) else n.intra=0
      vi.inter=fac.inter

      if(!is.na(covariante[1])){
	orden.b=list( ); orden.w=list( )
	if(n.inter > 0){
	  for(j in 1:n.inter){
	    orden.b[[j]]=c(nlevels(datos[,fac.inter[j]]),1)
	  }
	  names(orden.b)=fac.inter;
	}
	if(n.intra > 0){
	  for(j in 1:n.intra){
	    orden.w[[j]]=c(length(fac.intra[[j]]),1)
	  }
	names(orden.w)=names(fac.intra)
	}
	col.cov= match(covariante, names(datos))
	if(is.na(col.cov[1])) { cat('',fill=TRUE) ; cat('*** Error. Has incluido una covariante que no existe en los datos introducidos ***',fill=TRUE);
	  cat('',fill=TRUE)
	stop( ) }
      }

	# EXISTE VD
	if(!is.na(vd)){
		nombres=names(datos)
		if(is.numeric(vd)) vd=nombres[vd]
		# EXISTEN LAS VARIABLES
		check=existe.variable.fnc(datos,vd)
		if(check$cc !=0) {
			cat('',fill=TRUE)
			cat('*** Error. No existe la variable dependiente:',check$faltan,fill=TRUE)
			cat('*** en la base de datos incluida. Revisa el argumento vd',fill=TRUE)
		stop( )
		}
	}
	# FIN EXISTE VD

 if(is.na(silente[1])){

       if(n.inter > 1 & tipo==3 | (n.inter>=1 & n.intra >=1 & tipo==3)){
       cat('',fill=TRUE)
       cat('*** ---------------------------------- WARNING ---------------------------------- ***',fill=TRUE)
       cat('*** Has solicitado Anova con Suma de Cuadrados tipo III, si dispones de un numero ***',fill=TRUE)
       cat('*** de sujetos diferente por niveles de los factores intergrupo, indica tipo=2,  ***',fill=TRUE)
       cat('*** como argumento de la funcion.                                                 ***',fill=TRUE)
       cat('',fill=TRUE)
       }
  } # CIERRA SILENTE

 	# COMPLETAMENTE REPETIDO
	if(is.na(vi.inter)[1]){
	        tipo=3
	        if(is.na(col.empieza.mr[1]) & is.na(silente)){
		  cat('',fill=TRUE)
		  cat('*** Error. Tienes factores de medidas repetidas y no has incluido el argumento ***',fill=TRUE)
		  cat('*** col.empieza.mr. Ej: col.empieza.mr=3                                       ***',fill=TRUE)
		  cat('',fill=TRUE)
		 stop( )
		}
 		csmr=col.empieza.mr
		modo.intra=datos;
		check.fac.intra.fnc(fac.intra)
		idata=crea.idata.fnc(fac.intra);
		idesign= crea.idesign.fnc(idata)
		matriz=paste('as.matrix(','modo.intra[,csmr:(csmr+(dim(idata)[1])-1)]',')',sep='')
		if(is.na(covariante)){
			unelo=eval(parse(text=paste(matriz,'~','1',sep='')))
			modelo.lm.1=lm(unelo,data=modo.intra)
			resultado=Anova(modelo.lm.1, idata=idata,
			idesign=idesign,type='III', multivariate=FALSE)
			if(is.na(silente[1])) print(summary(resultado,multivariate=FALSE))
		}else{
			unelo=eval(parse(text=paste(matriz,'~',covariante,sep='')))
			modelo.lm.1=lm(unelo,data=modo.intra)
			resultado=Anova(modelo.lm.1, idata=idata,
			idesign=idesign,type='III', multivariate=FALSE)
			if(is.na(silente[1])) print(summary(resultado,multivariate=FALSE))
			apila=apila.los.datos.fnc(datos,fac.intra=fac.intra, fac.inter=NA,
				  col.empieza.mr=col.empieza.mr, covariante=covariante, silente=TRUE)
			if(n.intra <= 2){
			  print(grafica.panel.fnc(apila, que.factor=covariante, vd=vd, x.panel=names(fac.intra[1]),
			    		titulo = paste('Linealidad de ',covariante,' con vd',sep=''),
					orden=orden.w[[1]], regresion=TRUE) )
			}
			if(n.intra == 2){
			  print(grafica.panel.fnc(apila, que.factor=covariante, vd=vd, x.panel=names(fac.intra)[2],
			    		titulo = paste('Linealidad de ',covariante,' con vd',sep=''),
					orden=orden.w[[2]], regresion=TRUE) )
			}
		}
	} # CIERRE COMPLETAMENTE REPETIDO

	# ANOVA COMPLETAMENTE ALEATORIO
	vi.inter=fac.inter
	if(is.na(fac.intra[1])){
		if(is.na(vd[1])){
			cat('*** Has omitido la variable dependiente del modelo, incluye vd ***',fill=TRUE)
			cat('*** y la variable correspondiente como argumento en la funcion ***',fill=TRUE)
			stop( )
		}
		n.fac.inter=length(vi.inter)
		if(n.fac.inter==1) {
		   if(is.na(covariante[1])){
		      modelo=paste(vd,' ~ ',vi.inter[1],sep='')
		   }else{
		      modelo=paste(vd,' ~ ',covariante[1],'+',vi.inter[1],sep='')
		      if(!is.na(cova.x.vi[1])) modelo=paste(vd,' ~ ',covariante,'*',cova.x.vi,sep='')
		      print(grafica.panel.fnc(datos, que.factor=covariante, vd=vd, x.panel=fac.inter,
			    		titulo = paste('Linealidad de ',covariante,' con vd',sep=''),
			  		orden=orden.b[[1]], regresion=TRUE) )
		      X11()
		   }
		   levene=prueba.levene.fnc(datos, vd=vd, fac.inter[1])
		   levene=list(levene)
		   names(levene)=fac.inter
		}
		if(n.fac.inter==2) {
		   if(is.na(covariante[1])){
		      modelo=paste(vd,' ~ ',vi.inter[1],'*',vi.inter[2],sep='')
		   }else{
		      modelo=paste(vd,' ~ ',covariante[1],'+',vi.inter[1],'*',vi.inter[2],sep='')
		      if(!is.na(cova.x.vi[1])){
		        que.fac=match(cova.x.vi,fac.inter)
			cova.x.vi=paste(covariante,'*',fac.inter[que.fac],sep='')
			modelo=paste(vd,' ~ ',cova.x.vi,'+',vi.inter[1],'*',vi.inter[2],sep='')
		      }
		      print(grafica.panel.fnc(datos, que.factor=covariante, vd=vd, x.panel=fac.inter[1],
			    		titulo = paste('Linealidad de ',covariante,' con vd',sep=''),
			  		orden=orden.b[[1]], regresion=TRUE) )
		      print(grafica.panel.fnc(datos, que.factor=covariante, vd=vd, x.panel=fac.inter[2],
			    		titulo = paste('Linealidad de ',covariante,' con vd',sep=''),
			  		orden=orden.b[[2]], regresion=TRUE) )
		   }
		   levene=prueba.levene.fnc(datos, vd=vd, fac.inter)
		}
		if(n.fac.inter==3) {
		    if(is.na(covariante[1])){  
		      modelo=paste(vd,' ~ ',vi.inter[1],'*',vi.inter[2],'*',vi.inter[3],sep='')
		    }else{
		      modelo=paste(vd,' ~ ',covariante[1],'+',vi.inter[1],'*',vi.inter[2],'*',vi.inter[3],sep='')
		    }
		    levene=prueba.levene.fnc(datos, vd=vd, fac.inter)
		}
		if(tipo==3)
		    resultado=Anova(lm(eval(parse(text=modelo)),data=datos),type=3)
   		if(tipo==2)
		    resultado=Anova(lm(eval(parse(text=modelo)),data=datos),type=2)
		if(is.na(silente[1])){
		    print(resultado)
		    cat('',fill=TRUE)
		    print(levene)
		}
	} # CIERRE COMPLETAMENTE ALEATORIO

 	# SPLIT-PLOT
	if(!is.na(fac.intra[1]) & !is.na(vi.inter[1])) {
		vi.inter=fac.inter
 		csmr=col.empieza.mr
		modo.intra=datos;
		idata=crea.idata.fnc(fac.intra);
		idesign= crea.idesign.fnc(idata)
		n.inter=length(vi.inter)
		matriz=paste('as.matrix(','modo.intra[,csmr:(csmr+(dim(idata)[1])-1)]',')',sep='')

		# SI O NO COVARIANTE
		if(is.na(covariante)){
 			unelo=vi.inter[1]
			if(n.inter > 1){
 				for(i in 2:n.inter)
					unelo=paste(unelo,fac.inter[i],sep='*',collapse='*')
			}
 			unelo=paste(matriz,'~',unelo,sep='')	
			modelo.lm.1=lm(eval(parse(text=unelo)),data=datos)
			if(tipo==3)
				resultado=Anova(modelo.lm.1, idata=idata,
					idesign=idesign,type='III', multivariate=FALSE)
			if(tipo==2)
				resultado=Anova(modelo.lm.1, idata=idata,
					idesign=idesign,type='II', multivariate=FALSE)
			if(is.na(silente[1])) print(summary(resultado,,multivariate=FALSE))
		}else{
		# SI NO O SI COVA.x.VI
			if(is.na(cova.x.vi[1])){
 				unelo=paste(covariante,'+',vi.inter[1],sep='')
				if(n.inter > 1){
 					for(i in 2:n.inter)
						unelo=paste(unelo,fac.inter[i],sep='*',collapse='*')
				}
 				unelo=paste(matriz,'~',unelo,sep='')
			}else{
				falta=fac.inter[-(match(cova.x.vi, fac.inter))]
 				unelo=paste(covariante,'+',covariante,':',cova.x.vi,'+',cova.x.vi,sep='')
				if(n.inter > 1){
					for(i in 1:length(falta))
						unelo=paste(unelo,falta[i],sep='*',collapse='+')
				}
 				unelo=paste(matriz,'~',unelo,sep='')
			}
			datos.st=apila.los.datos.fnc(datos,fac.intra=fac.intra, fac.inter=fac.inter,
			      covariante=covariante, col.empieza.mr=col.empieza.mr ,silente=TRUE)

			for(i in 1:n.inter){
				print(grafica.panel.fnc(datos.st, que.factor=covariante, vd='vd', x.panel=fac.inter[i],
		    			titulo = paste('Linealidad de ',covariante,' con vd',sep=''),
			     		orden=orden.b[[i]], regresion=TRUE) )
			}

			for(i in 1:n.intra){
				print(grafica.panel.fnc(datos.st, que.factor=covariante, vd='vd', x.panel=names(fac.intra)[i],
			    		titulo = paste('Linealidad de ',covariante,' con vd',sep=''),
					orden=orden.w[[i]], regresion=TRUE) )
			}
			modelo.lm.1=lm(eval(parse(text=unelo)),data=modo.intra)
			if(tipo==3)
				resultado=Anova(modelo.lm.1, idata=idata,
					idesign=idesign,type='III', multivariate=FALSE)
			if(tipo==2)
				resultado=Anova(modelo.lm.1, idata=idata,
					idesign=idesign,type='II', multivariate=FALSE)
			if(is.na(silente[1])) print(summary(resultado,multivariate=FALSE))
			}

	} # CIERRE SPLIT-PLOT

  if(is.na(silente[1])){
	cat('',fill=TRUE)
	salida=calcula.eta.power.MC.fnc(datos,col.empieza.mr=col.empieza.mr,resultado,
	  fac.intra=fac.intra,fac.inter=fac.inter,tipo,vd, covariante,cova.x.vi)

	tablas=crea.tabla.medias.fnc(datos,fac.inter,fac.intra,col.empieza.mr,vd, 
	    covariante=covariante)

	pow=calcula.potencia.efecto.fnc(tablas,salida,n.inter,n.intra)
	fase1=cbind(salida[[1]],pow)
	row.names(fase1)=row.names(pow)
	salida[[1]]=fase1

	print(salida)

	crea.cat.fnc('DESCRIPTIVOS')

	print(tablas)
	res=list(tablas=tablas, salida=salida)
	if(grafica){
	  if(is.na(lim[1])) lim=1
	  grafica.ic.fnc(tablas,MCintra=salida[[2]],gli=salida[[3]],ylim=ylim,lim=lim,to.pdf=to.pdf,
		size.font,apaisado,nombre, color)
	}

	if(is.na(poshoc[1]) & !is.na(contrastes[1])){
		cat('',fill=TRUE)
		cat('*** WARNING. Has incluido contrastes ortogonales pero no has indicado sobre',fill=TRUE)	
		cat('*** que factor deseas aplicarlo. Incluye el argumento poshoc con el nombre',fill=TRUE)	
		cat('*** del factor sobre el que desas aplicar la lista de contrastes.',fill=TRUE)	
		cat("*** Ej. poshoc='tratamiento', contrastes=mis.contrastes)",fill=TRUE)	
		cat('',fill=TRUE)
		cat('',fill=TRUE)
	}	
	
	# POSHOC
        if(!is.na(poshoc[1])){
 	  if(n.intra==0) {
	    dat.apilados=datos
		dat.apilados$sujeto=paste('suj',1:dim(dat.apilados)[1],sep='')
	  }else{
	    dat.apilados=apila.los.datos.fnc(datos, fac.inter=fac.inter, fac.intra=fac.intra,
		  col.empieza.mr=col.empieza.mr,silente=TRUE)
          }
          if(!is.na(contrastes[1]) & length(poshoc) > 1){
	    cat('', fill=TRUE)
	    cat('*** Error. Cuando solicitas contrastes ortogonales o de tendencias        ***',fill=TRUE)
	    cat('*** el numero de factores a contrastar en el argumento poshoc debe ser 1. ***',fill=TRUE)
	    cat("*** Ej: Anova.fnc(datos, fac.inter=fac.inter, poshoc='tratamiento', ",fill=TRUE)
	    cat("*** 			contrastes='tendencia')" , fill=TRUE)
	    cat('', fill=TRUE)
	    stop( )
	  }

	  # CHEQUEA QUE LOS CONTRASTES SON CORRECTOS
	  if(!is.na(contrastes[1]))
		check.contrastes.fnc(datos,poshoc,fac.inter, fac.intra, contrastes)

	  for(i in 1:length(poshoc)){
	    if(n.intra==0) apilados=TRUE
	    x=poshoc[i]
	    n.tipo=strsplit(x,':')[[1]]
	    factores=c(fac.inter,names(fac.intra))
	    chivato=match(n.tipo,factores)
	    if(sum(is.na(chivato))!=0){
		 es.error=n.tipo[is.na(chivato)]
		 cat('',fill=TRUE)
		 cat('*** Error. El factor:',es.error,' incluido como nombre de la interaccion',fill=TRUE)
		 cat('*** no es un nombre de factor de tu estudio. Por favor, compruebalo.    ',fill=TRUE)
		 cat('',fill=TRUE)
		stop( )
	    }
 					
	    if(length(n.tipo)==1) {
	      crea.cat.fnc('POSHOC')
	      print(contrastes.poshoc.fnc(dat.apilados, vd=vd, que.factor=x,fac.inter=fac.inter,
		fac.intra=fac.intra,contrastes=contrastes,silente=TRUE))
	    }
	    if(length(n.tipo)==2) {
	      crea.cat.fnc('EFECTOS SIMPLES')
	      print(efectos.simples.fnc(dat.apilados,interaccion=x,vd=vd,fac.inter=fac.inter, color=color,
		fac.intra=fac.intra,silente=TRUE, etiqueta=etiqueta, apilados=apilados))
	    }
     	if(length(n.tipo)==3) {
		  crea.cat.fnc('INTERACCION TRIPLE')
		  print(interaccion.triple.fnc(dat.apilados,interaccion=x,vd=vd,fac.inter=fac.inter, color=color,
			fac.intra=fac.intra,silente=TRUE, etiqueta=etiqueta))
		}
	  }
	 }
         # FIN POSHOC

	#try(detach(package:car),silent=TRUE)

       if(n.inter > 1 & tipo==3 | (n.inter>=1 & n.intra >=1 & tipo==3)){
       cat('',fill=TRUE)
       cat('*** ---------------------------------- WARNING ---------------------------------- ***',fill=TRUE)
       cat('*** Has solicitado Anova con Suma de Cuadrados tipo III, si dispones de un numero ***',fill=TRUE)
       cat('*** de sujetos diferente por niveles de los factores intergrupo, indica tipo=2,  ***',fill=TRUE)
       cat('*** como argumento de la funcion.                                                 ***',fill=TRUE)
       cat('',fill=TRUE)
       }

   #return(res)
   }else{
      return(resultado)
   } # FIN SILENTE
 }
# -------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
# A partir de las tablas y resultados de salida del Anova.fnc se estima la
# potencia observada de cada efecto. Esta funcion que se ejecuta dentro de
# Anova, llama a pot.obs.anova.fnc( )
# -------------------------------------------------------------------------------
 calcula.potencia.efecto.fnc=function(tablas,salida,n.inter,n.intra){
	# LEE LAS TABLAS DE MEDIAS,N, MCE y GL
	MCw=salida[[2]]
 	gl= salida[[3]]
	tab=tablas$t.medias
	tab.n=tablas$t.n
	nombres=names(tab)

	# ADAPTAMOS LA SALIDA DE MCE PARA CIERTOS TIPOS DE DESIGN
	if(n.inter==2 & n.intra==0){
		MCw=rep(salida[[2]],3)
 		gl=rep(salida[[3]],3)
 	}
	if(n.inter==3 & n.intra==0){
		MCw=rep(salida[[2]],6)
 		gl=rep(salida[[3]],6)
 	}
	if(n.inter==2 & n.intra==2){
		MCw=salida[[2]]
		MCw=MCw[-7]
		gl=salida[[3]]
		gl=gl[-7]
	} # FIN ADAPTACION

	# LOOP DE CREACION DE POTENCIA PARA CADA EFECTO
	alma=data.frame( )
	for(i in 1:length(tab)){
		size=dim(tab[[i]])
		lab=nombres[i]
		divide=strsplit(lab,':'); divide=divide[[1]]
 		if(length(divide) == 2){
			gl.1=(size[1]-1)*(size[2]-1)
			lista=list(tab[[divide[1]]],tab[[divide[2]]],tab[[i]])
		}else{
			gl.1=size-1
			lista=tab[[i]]
		}
		MErr=as.numeric(MCw[i])
		n=ceiling(mean(tab.n[[i]]))
		gl.t=c(gl.1,gl[i])
		pow=pot.obs.anova.fnc(lista,MErr,n,gl.t)
		alma=rbind(alma,c(pow$pnc[1],pow$Pot.obs[1]))
 	}
 	names(alma)=c('pnc','power.obs')
 	alma=round(alma,4)
 	row.names(alma)=nombres
 	alma
 }
# -------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
# Calcula la potencia observada de un efecto a partir de la matriz de medias
# y MCintra, n y gl introducidos como argumentos.
# -------------------------------------------------------------------------------
 pot.obs.anova.fnc=function(medias,MCError,n,gl){
	# SI ES INTERACCION O NO
	if(is.list(medias)){
		J=dim(medias[[1]]); K=dim(medias[[2]])
		medias.j=medias[[1]]; medias.k=medias[[2]];
		medias.jk=medias[[3]]; gm=mean(medias.j)

		n.medias=length(medias)
		n.MCw=length(MCError)
		if(n.MCw==1) MCError=rep(MCError,n.medias)

		acumula=0
		for (j in 1:J){
			for (k in 1:K) {
				acumula=acumula+(medias.jk[j,k]-medias.j[j]-medias.k[k]+gm)^2
			}
		}
		pnc=as.numeric( (acumula*n)/MCError);
	} else {
		n=mean(n)
		pnc= n * sum( (medias- mean(medias))^2)/MCError
	}
	gl1=gl[1]
	gl2=gl[2]
	F.h0.95=qf(0.95, gl1, gl2)
	potencia= 1 - pf(F.h0.95, gl1, gl2, pnc)
 return(list(Confianza=0.95, n=n, pnc=pnc, Pot.obs=potencia))
 }
# -------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
#
# -------------------------------------------------------------------------------
 check.fac.intra.fnc=function(fac.intra){
  if(!is.na(fac.intra[1]) & !is.list(fac.intra)){
    cat('',fill=TRUE)
    cat('*** Error. El objeto fac.intra debe ser obligatoriamente una lista.',fill=TRUE)
    cat("*** Por ejemplo para un design completamente repetido 2 x 2 podria ser: ",fill=TRUE)
    cat('',fill=TRUE)
    cat("*** Ej. fac.intra = list(facA=c('A1','A2'), facB=c('B1','B2'))",fill=TRUE)
    cat('',fill=TRUE)
    cat("*** Se trata de una lista cuyos nombres son los que deseamos",fill=TRUE)
    cat("*** para los factores (facA y facB y dentro de ellos los J y K",fill=TRUE)
    cat("*** nombres de los niveles de medidas repetidas entre comillas.",fill=TRUE)
    cat("*** o apostrofes. Todos los elementos de la lista estan separados",fill=TRUE)
    cat("*** por el caracter coma (,).",fill=TRUE)
    cat('',fill=TRUE)
    stop()
  } 
 } 
# -------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
# FUNCION. Crea la data.frame idata que contiene en su interior el
# my.nesting de las medidas repetidas para ser tratados los datos
# en modo intra con la libreria car.
# crea.idata.fnc(my.levels.fac.intra)
# Entra una lista con los nombres y niveles de los factores del design
# my.levels.fac=list(facA=c('A1','A2'),facB=c('B1','B2','B3'))
# -------------------------------------------------------------------------------
 crea.idata.fnc=function(my.levels.within){
	my.factors.within=names(my.levels.within)
	nfac=length(my.factors.within);
	n.my.levels=matrix(0,length(my.factors.within))
	for (i in 1:nfac) n.my.levels[i]=length(my.levels.within[[i]])
      if (nfac == 1) {
		idata=data.frame(
		FA=factor(rep(my.levels.within[[1]],1), levels=my.levels.within[[1]]))
  		names(idata)=my.factors.within
	}
      if (nfac == 2) {
		idata=data.frame(
		FA=factor(rep(my.levels.within[[1]],each=n.my.levels[2]),
		        levels=my.levels.within[[1]]),
		        FB=factor(rep(my.levels.within[[2]],n.my.levels[1]),
		        levels=my.levels.within[[2]]))
	names(idata)=my.factors.within;
      }
      if (nfac == 3) {
		idata=data.frame(
		FA=factor(rep(my.levels.within[[1]],each=n.my.levels[2]*n.my.levels[3]),
		    levels=my.levels.within[[1]]),
		FB=factor(rep(rep(my.levels.within[[2]],each=n.my.levels[3],n.my.levels[1]),
		    levels=my.levels.within[[2]])),
		FC=factor(rep(my.levels.within[[3]],n.my.levels[1]*n.my.levels[2]),
		levels=my.levels.within[[3]]))
	names(idata)=my.factors.within
	}
      if (nfac == 4) {
		idata=data.frame(
		FA=factor(rep(my.levels.within[[1]],each=n.my.levels[2]*n.my.levels[3]*
				n.my.levels[4]),levels=my.levels.within[[1]]),
		FB=factor(rep(rep(my.levels.within[[2]],each=n.my.levels[3]*
				n.my.levels[4],n.my.levels[1]),levels=my.levels.within[[2]])),
		FC=factor(rep(rep(my.levels.within[[3]],each=n.my.levels[4]),
				n.my.levels[1]*n.my.levels[2]),levels=my.levels.within[[3]]),
		FD=factor(rep(my.levels.within[[4]],n.my.levels[1]*n.my.levels[2]*n.my.levels[3]),
				levels=my.levels.within[[4]]))
	names(idata)=my.factors.within
      }
     if (nfac == 5) {
		idata=data.frame(
		FA=factor(rep(my.levels.within[[1]],each=n.my.levels[2]*n.my.levels[3]*
				n.my.levels[4]*n.my.levels[5]),levels=my.levels.within[[1]]),
		FB=factor(rep(rep(my.levels.within[[2]],each=n.my.levels[3]*n.my.levels[4]*
				n.my.levels[5],n.my.levels[1]),levels=my.levels.within[[2]])),
		FC=factor(rep(rep(my.levels.within[[3]],each=n.my.levels[4]*n.my.levels[5]),
				n.my.levels[1]*n.my.levels[2]),levels=my.levels.within[[3]]),
		FD=factor(rep(rep(my.levels.within[[4]],each=n.my.levels[5]),n.my.levels[1]*
				n.my.levels[2]*n.my.levels[3]),levels=my.levels.within[[4]]),
		FE=factor(rep(my.levels.within[[5]],n.my.levels[1]*n.my.levels[2]*n.my.levels[3]*
				n.my.levels[4]),levels=my.levels.within[[5]]))
	names(idata)=my.factors.within
      }
     if (nfac == 6) {
		idata=data.frame(
		FA=factor(rep(my.levels.within[[1]],each=n.my.levels[2]*n.my.levels[3]*
				n.my.levels[4]*n.my.levels[5]*n.my.levels[6]),
					levels=my.levels.within[[1]]),
		FB=factor(rep(rep(my.levels.within[[2]],each=n.my.levels[3]*n.my.levels[4]*
				n.my.levels[5]*n.my.levels[6],	n.my.levels[1]),
					levels=my.levels.within[[2]])),
		FC=factor(rep(rep(my.levels.within[[3]],each=n.my.levels[4]*n.my.levels[5]*
				n.my.levels[6]),n.my.levels[1]*n.my.levels[2]),
					levels=my.levels.within[[3]]),
		FD=factor(rep(rep(my.levels.within[[4]],each=n.my.levels[5]*n.my.levels[6]),
				n.my.levels[1]*n.my.levels[2]*n.my.levels[3]),
					levels=my.levels.within[[4]]),
		FE=factor(rep(rep(my.levels.within[[5]],each=n.my.levels[6]),n.my.levels[1]*
				n.my.levels[2]*n.my.levels[3]*n.my.levels[4]),
					levels=my.levels.within[[5]]),
		FG=factor(rep(my.levels.within[[6]],n.my.levels[1]*n.my.levels[2]*n.my.levels[3]*
				n.my.levels[4]*n.my.levels[5]),levels=my.levels.within[[6]]))
	names(idata)=my.factors.within
      }
 return(idata)
 }
# -------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
# FUNCION. Crea la estructura de analisis intra para las variables
# contenidas en la idata. La salida es un eval(parse(text=)) para
# la funcion Anova de car
# -------------------------------------------------------------------------------
 crea.idesign.fnc=function(idata) {
	nfac=dim(idata)[[2]]
      if (nfac > 6) return(cat('*** El maximo numero de my.factors intra es 6 ***'))
      rm.my.factors=names(idata)
      if (nfac==1) {
	   idesign=eval(parse(text=paste('~ ',rm.my.factors,sep='')))
      }
      if (nfac==2) {
	   idesign=eval(parse(
		text=paste('~ ',rm.my.factors[1],'*',rm.my.factors[2],sep='')))
		}
      if (nfac==3) {
	   idesign=eval(parse(
		text=paste('~ ',rm.my.factors[1],'*',rm.my.factors[2],
			'*',rm.my.factors[3],sep='')))
		}
      if (nfac==4) {
	   idesign=eval(parse(
		text=paste('~ ',rm.my.factors[1],'*',rm.my.factors[2],
			'*',rm.my.factors[3],'*',rm.my.factors[4],sep='')))
		}
      if (nfac==5) {
	   idesign=eval(parse(
		text=paste('~ ',rm.my.factors[1],'*',rm.my.factors[2],
			'*',rm.my.factors[3],'*',rm.my.factors[4],'*',rm.my.factors[5],sep='')))
		}
      if (nfac==6) {
	   idesign=eval(parse(
		text=paste('~ ',rm.my.factors[1],'*',rm.my.factors[2],
			'*',rm.my.factors[3],'*',rm.my.factors[4],'*',rm.my.factors[5],
			'*',rm.my.factors[6],sep='')))
		}

 return(idesign)
 }
# -------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
# Recontruye la matriz de datos apilada ajustada por la covariante. 
# -------------------------------------------------------------------------------
 data.poscovari.fnc=function(datos, vd=NA, fac.inter=NA, fac.intra=NA, covariante,
		col.empieza.mr=NA, apilados=FALSE){
		
	datos=centra.variable.fnc(datos, variable=covariante, silente=TRUE)
	covariante=paste('c.',covariante,sep='')
	n.inter=0; n.intra=0
	if(!is.na(fac.intra[1])) n.intra=length(names(fac.intra))
	if(!is.na(fac.inter[1])) n.inter=length(fac.inter)
	if(n.intra != 0){
		if(!apilados){
			datos.apilados= apila.los.datos.fnc(datos, 
				col.empieza.mr=col.empieza.mr,
				fac.intra=fac.intra, 
				fac.inter=fac.inter, 
				covariante=covariante, silente=T)
		}else{
			datos.apilados=datos
			nombres=names(datos.apilados)
			if(is.na(match('sujeto',nombres)) & !is.na(fac.intra[1])){
				cat('*** Error. Dices que tus datos estan apilados pero no existe la variable sujeto.',fill=TRUE)
				cat('*** Renombra apropiadamente la variable actual de sujeto por el nuevo nombre sujeto',fill=TRUE)
				cat('*** Puedes utilizar la funcion cambia.nombre.var.fnc ',fill=TRUE)
				cat("*** Ej: datos.apilados=cambia.nombre.var.fnc(datos.apilados, 'ID'='sujeto'", fill=TRUE)
				stop( )
			}
		}
	}
	
	if(n.intra==0 & !apilados) datos.apilados=datos
	if(n.inter !=0 & n.intra==0){
	   acumula=fac.inter[1]
	   for(i in 1:length(fac.inter)){
	    if(i==1) next
	    acumula=paste(acumula,fac.inter[i],sep='*',collapse=' + ')
	   }
	   modelo1=paste(vd,'~',covariante,' + ',acumula,sep='')
	}
	if(n.inter==0 & n.intra==1){
	    contrasts(datos.apilados[,names(fac.intra)[1]])=contr.treatment
	    modelo1=paste('vd ~ ',covariante,'*',names(fac.intra)[1],'+ (1|sujeto)',sep='')
	}
	if(n.inter==0 & n.intra==2){
	    for(i in 1:2) contrasts(datos.apilados[,names(fac.intra)[i]])=contr.treatment
	    modelo1=paste('vd ~ ',covariante,'*',names(fac.intra)[1],'*',
	      names(fac.intra)[2],'+ (1|sujeto)',sep='')
	}
	if(n.inter==1 & n.intra==1){
	    factores=c(fac.inter,names(fac.intra))
	    for(i in 1:2) contrasts(datos.apilados[,factores[i]])=contr.treatment
	    modelo1=paste('vd ~ ',covariante,' + ',fac.inter[1],' + ',
		    covariante,':',names(fac.intra)[1],' + ',
		    fac.inter[1],'*',names(fac.intra)[1],'+ (1|sujeto)',sep='')
	}
	if(n.inter==2 & n.intra==1){
	    factores=c(fac.inter,names(fac.intra))
	    for(i in 1:length(factores)) contrasts(datos.apilados[,factores[i]])=contr.treatment
	    modelo1= paste('vd ~ ',covariante,' + ',fac.inter[1],' + ',
	      fac.inter[2],' + ',
	      fac.inter[1],':',fac.inter[2],' + ',
	      names(fac.intra)[1],' + ',
	      covariante,':',names(fac.intra)[1],' + ',
	      fac.inter[1],'*',names(fac.intra)[1],' + ',
	      fac.inter[2],'*',names(fac.intra)[1],' + ',
	      fac.inter[1],':',fac.inter[2],':',names(fac.intra)[1],'+ (1|sujeto)',sep='')
	}
	if(n.inter==1 & n.intra==2){
	    factores=c(fac.inter,names(fac.intra))
	    for(i in 1:length(factores)) contrasts(datos.apilados[,factores[i]])=contr.treatment
	    modelo1= paste('vd ~ ',covariante,' + ',
	      fac.inter[1],' + ',
	      names(fac.intra)[1],' + ',
	      covariante,':',names(fac.intra)[1],' + ',
	      fac.inter[1],':',names(fac.intra)[1],' + ',
	      names(fac.intra)[2],' + ',
	      covariante,':',names(fac.intra)[2],' + ',
	      fac.inter[1],':',names(fac.intra)[2],' + ',
	      names(fac.intra)[1],':',names(fac.intra)[2],' + ',
	      covariante,':',names(fac.intra)[1],':',names(fac.intra)[2],' + ',
	      fac.inter[1],':',names(fac.intra)[1],':',names(fac.intra)[2],'+ (1|sujeto)',sep='')
	}
	if(n.inter==2 & n.intra==2){
	    factores=c(fac.inter,names(fac.intra))
	    for(i in 1:length(factores)) contrasts(datos.apilados[,factores[i]])=contr.treatment	
	    modelo1=paste('vd ~ ',covariante,' + ',
	      fac.inter[1],' + ',
	      fac.inter[2],' + ',
	      fac.inter[1],':',fac.inter[2],' + ',
	      names(fac.intra)[1],' + ',
	      covariante,':',names(fac.intra)[1],' + ',
	      fac.inter[1],':',names(fac.intra)[1],' + ',
	      fac.inter[2],':',names(fac.intra)[1],' + ',
	      fac.inter[1],':',fac.inter[2],':',names(fac.intra)[1],' + ',
	      names(fac.intra)[2],' + ',
	      covariante,':',names(fac.intra)[2],' + ',
	      fac.inter[1],':',names(fac.intra)[2],' + ',
	      fac.inter[2],':',names(fac.intra)[2],' + ',
	      fac.inter[1],':',fac.inter[2],':',names(fac.intra)[2],' + ',
	      names(fac.intra)[1],':',names(fac.intra)[2],' + ',
	      covariante,':',names(fac.intra)[1],':',names(fac.intra)[2],' + ',
	      fac.inter[1],':',names(fac.intra)[1],':',names(fac.intra)[2],' + ',
	      fac.inter[2],':',names(fac.intra)[1],':',names(fac.intra)[2],' + ',
	      fac.inter[1],':',fac.inter[2],':',names(fac.intra)[1],':',names(fac.intra)[2],    
		'+ (1|sujeto)',sep='')
	}

	if(n.intra!=0){
		datos.apilados=na.omit(datos.apilados)
		suppressWarnings(require(lme4, quietly = TRUE))
		mod.lmer=lmer(eval(parse(text=modelo1)),data=datos.apilados)	
		suj.w=model.matrix(mod.lmer)
		nombres_=names(fixef(mod.lmer))
		colnames(suj.w)=nombres_
		lista=list()
		for(i in 1:length(nombres_)){
		  lista[[i]]=do.call(rbind,strsplit(nombres_[i],':'))
		  lista[[i]]=lista[[i]][1]
		}
		lista=as.character(do.call(rbind,lista))
		indice=lista %in% covariante
		suj.w[,indice]=0
		datos.apilados$pred=suj.w%*%fixef(mod.lmer)		
		try(detach(package:lme4), silent=TRUE)
	}else{
		datos.apilados=na.omit(datos.apilados)	
		datos.apilados$index=1:dim(datos.apilados)[1]
		datos.apilados=datos.apilados[,c('index',vd,fac.inter,covariante)]
		indice=complete.cases(datos.apilados)
		if(sum(!indice)!=0){
		  completos=datos.apilados[indice,]
		  incompletos=datos.apilados[!indice,]
		  incompletos$pred=NA
		  incompletos$pred=as.numeric(incompletos$pred)
		}else{
		  completos=datos.apilados
		}
		mod.lm=lm(eval(parse(text=modelo1)), data=completos)
		suj.w=model.matrix(mod.lm)
		suj.w[,2]=0
		completos$pred=as.numeric(suj.w%*%coef(mod.lm))
		if(sum(!indice)!=0){
		  completos=rbind(completos,incompletos)
		  completos=ordena.por.variable.fnc(completos,variable='index', silente=TRUE)
		}else{
		  datos.apilados=completos[,-1]
		}
	}
	try(detach(package:lme4),silent=TRUE)
 	return(datos.apilados)
 }
# -------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
#  mix.mod = mixed.model.fnc(lexde.cl,factores, random.sujeto=T, random.item=T,
#      covariante=NA, grafica=T)
# -------------------------------------------------------------------------------
mixed.model.fnc=function(datos=NA, fac.inter=NA,fac.intra=NA, random.sujeto=TRUE,
 		fac.random=NA,covariante=NA, random.item=FALSE,  tipo=3, grafica=FALSE,poshoc=NA, 
 		silente=FALSE, latex=FALSE, accuracy=FALSE){
 		
	if(class(try(is.na(datos)))=='logical') {
		cat('',fill=TRUE)
		crea.cat.fnc('mixed.model.fnc')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat('*** Declaramos el design ***', fill=TRUE)
		cat("  fac.intra= list(mrA=c('low','hig'), mrB=c('short','long'))   ",fill=TRUE)
		cat('',fill=TRUE)
		cat('*** Apilamos los datos ***', fill=TRUE)
		cat("  datos.apilados = apila.los.datos.fnc(datos, fac.intra=fac.intra,   ",fill=TRUE)
		cat("  		col.empieza.item=1, n.item=172)    ",fill=TRUE)
		cat('',fill=TRUE)
		cat('*** Estimamos el modelo ***', fill=TRUE)
		cat("  mixed.model.fnc(datos.apilados, fac.intra=fac.intra,   ",fill=TRUE)    
		cat("  		random.sujeto=T, random.item=T)   ",fill=TRUE)    
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Lleva cabo la estimacion de modelos de efectos mixtos (fijos y aleatorios)', fill=TRUE)
		cat(' para el paradigma de design experimentales multifactoriales con sujetos e items.', fill=TRUE)
		cat('', fill=TRUE)
		cat(" sites.google.com/site/ullrtoolbox/07---analisis-multivariado/mixed-model-fnc", fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		print(argumentos('mixed.model.fnc'))
	return('help')
	}

	dat.st=datos
	dat.st$sujeto=as.factor(as.character(dat.st$sujeto))
	nombres=names(datos)
	chivato_='item' %in% nombres
	if(!chivato_ & random.item[1]){
		cat('',fill=TRUE)
		cat('*** Error. No existe la variable item en tu base de datos. ***',fill=TRUE)
		cat('*** Modifica random.item a FALSE.',fill=TRUE)
		stop()
		cat('',fill=TRUE)
	}
	if(chivato_ & random.item) dat.st$item=as.factor(as.character(dat.st$item))
	
    check.fac.intra.fnc(fac.intra)
    check.factores.fnc(fac.inter,fac.intra)
	check.niveles.fnc(datos, fac.inter=fac.inter, fac.intra=fac.intra)
    
	if(latex){
		cat('',fill=TRUE)
		cat('*** mixed.model.fnc no admite por el momento el argumento latex ***',fill=TRUE)
		cat('',fill=TRUE)
	}	
    try(detach(package:nlme),silent=TRUE) #detach de nlme si estuviese cargada

    #require(lme4, quietly = TRUE); require(car, quietly = TRUE)
    require(lmerTest, quietly = TRUE); require(car, quietly = TRUE)
    if(accuracy) crea.cat.fnc('MIXED LOGIT MODEL') else crea.cat.fnc('MIXED MODEL')

	# CHECK SI EXISTEN LOS FACTORES INTER
      if(!is.na(fac.inter[1])) {
		n.inter=length(fac.inter)
		ok.fac.inter=check.fac.inter(datos, fac.inter)
		if(length(ok.fac.inter)!=0)
			stop('*** No existe algun o algunos de los factores inter incluidos ***')
	 }else{
		n.inter=0
	 }
	# FIN CHECK SI EXISTEN LOS FACTORES INTER
	
	# CHECK SI EXISTE LA VARIABLE VD
	if (sum('vd' %in% names(dat.st))==0){
		cat('',fill=TRUE)
		cat('*** Error. No existe la variable obligatoria vd.             ***',fill=TRUE)
		cat('*** Si por ejemplo quisieras modelar el tiempo de reaccion,  ***',fill=TRUE)
		cat('*** deberas modificar el nombre origina de tr a vd.          ***',fill=TRUE)
		cat("*** Ej: datos=cambia.nombre.var.fnc(datos, 'tr','vd')        ***",fill=TRUE)
		cat('',fill=TRUE)
		stop()
	}
	
 # CHECK SI VD ES LOGIT
	tabla=data.frame(with(dat.st, table(vd)))
	if(dim(tabla)[1]==2 & !accuracy){
		cat('',fill=TRUE)
		cat('*** WARNING. Tu variable dependiente parece binomial (0,1).     ***',fill=TRUE)	
		cat('*** El resultado estimado es seguramente incorrecto.            ***',fill=TRUE)	
		cat('*** Incluye el argumento accuracy=T en la llamada a la funcion. ***',fill=TRUE)	
		cat('',fill=TRUE)
		cat('',fill=TRUE)
	}
	
	if(accuracy & !is.na(poshoc[1])){
		cat('',fill=TRUE)
		cat('',fill=TRUE)
		cat('*** WARNING. No pueden estimarse los contrastes poshoc en modelos binomiales  ***',fill=TRUE)		
		cat('',fill=TRUE)
		cat('',fill=TRUE)
	}
	
	
      if(!is.na(fac.intra[1])) n.intra=length(names(fac.intra)) else n.intra=0

      if(n.inter > 0 & n.intra > 0) factores=c(fac.inter,names(fac.intra))
      if(n.inter == 0 & n.intra > 0) factores=names(fac.intra)
      if(n.inter > 0 & n.intra==0) factores=fac.inter
      

  # ------------------------------------------------------------------
  # Check si algun factor tiene NA como valor y lo elimina
  chivato=list( )
  for(i in 1:length(factores)){
    sal1=dat.st[is.na(dat.st[,factores[i]]),]
    chivato[[i]]=dim(sal1)
    if(dim(sal1)[1]!=0) dat.st=dat.st[!is.na(dat.st[,factores[i]]),]
  }
  names(chivato)=factores
  for(i in 1:length(chivato)){
    if(chivato[[i]][1]!= 0){
      cat('',fill=TRUE)
      cat('*** WARNING. El factor.',factores[i],'tiene NA como valores en algunos registros. ***',fill=TRUE)
      cat('*** Esos registros han sido eliminados antes de realizar el analisis.             ',fill=TRUE)
      cat('',fill=TRUE)
    }
  }   			
  # FIN CREA CONDICION
  # ------------------------------------------------------------------

      if(n.intra!=0){
	 	for(i in 1:length(fac.intra))
	    	if(!is.factor(dat.st[,names(fac.intra[i])]))
	      		dat.st=reordena.factor.fnc(dat.st,que.factor=names(fac.intra)[i],
					niveles=fac.intra[[i]],silente=TRUE) 
      }      			 	
      n.factores=length(factores)

    #-------------------------------------------------------------------------------
    # CONTROLA COVARIANTE
    #-------------------------------------------------------------------------------
    if(is.na(covariante[1])){ # Si no hay covariante
	if(n.factores==1) { modelo1=paste('vd','~',factores[1],'+',sep='')
	    mod.lis=paste('vd','~',factores[1],' | sujeto',sep='')}
	if(n.factores==2) { modelo1=paste('vd','~',factores[1],'*',factores[2],'+',sep='')
	    mod.lis=paste('vd','~',factores[1],'*',factores[2],' | sujeto',sep='')}
	if(n.factores==3) { modelo1=paste('vd','~',factores[1],'*',factores[2],'*',
	    factores[3],'+',sep='')
	    mod.lis=paste('vd','~',factores[1],'*',factores[2],'*',factores[3],' | sujeto',sep='')}
	if(n.factores==4) { modelo1=paste('vd','~',factores[1],'*',factores[2],'*',
	      factores[3],'*',factores[4],'+',sep='')
	    mod.lis=paste('vd','~',factores[1],'*',factores[2],'*',factores[3],'*',
	    factores[4],' | sujeto',sep='')}

	rand0='(1|sujeto)';
	modelo00=paste('vd','~ 1 + ',rand0,sep='')
	modelo0=paste(modelo1,rand0,sep='')
	if(!is.na(fac.random[1])){
	  rand1=paste('(',fac.random[1],'|sujeto)',sep='')
	  modelo1=paste(modelo1,rand1,sep='')
	}else{
	  modelo1=modelo0
	}
      }else{ # Si hay covariante
	if(n.factores==1) modelo1=paste('vd','~',covariante,'+',factores[1],'+',sep='')
	if(n.factores==2) modelo1=paste('vd','~',covariante,'+',factores[1],'*',factores[2],'+',sep='')
	if(n.factores==3) modelo1=paste('vd','~',covariante,'+',factores[1],'*',factores[2],'*',
	      factores[3],'+',sep='')
	if(n.factores==4) modelo1=paste('vd','~',covariante,'+',factores[1],'*',factores[2],'*',
	      factores[3],'*',factores[4],'+',sep='')
	rand0='(1|sujeto)';
	modelo00=paste('vd','~ 1 + ',rand0,sep='')
	modelo0=paste(modelo1,rand0,sep='')
	if(!is.na(fac.random[1])){
	  rand1=paste('(',fac.random[1],'|sujeto)',sep='')
	  modelo1=paste(modelo1,rand1,sep='')
	}
    } # FIN DE COVARIANTE en random sujeto
    #-------------------------------------------------------------------------------

    # ESTIMA MODELO LMER COMUN A TODOS PARA RANDOM SUJETO Y FACTOR EN SUJETO
    if(!accuracy){
		mod.lmer00=lmer(eval(parse(text=modelo00)),data=dat.st)
		mod.lmer0=lmer(eval(parse(text=modelo0)),data=dat.st)
	}else{
		mod.lmer00=glmer(eval(parse(text=modelo00)),data=dat.st,
			family=binomial(link='logit'))
		mod.lmer0=glmer(eval(parse(text=modelo0)),data=dat.st,
			family=binomial(link='logit'))	
	}
      
    if(!is.na(fac.random[1])){
	 	if(!accuracy){
			mod.lmer1=lmer(eval(parse(text=modelo1)),data=dat.st)
	 	}else{
			mod.lmer1=glmer(eval(parse(text=modelo1)),data=dat.st,
				family=binomial(link='logit'))
		}
	}
	
    # GRAFICAS DE PENDIENTES
    if(n.intra==1){names.w=names(fac.intra)
	    mod.lis=paste('vd','~',names.w[1],' | sujeto',sep='')}
    if(n.intra==2){names.w=names(fac.intra);
	    mod.lis=paste('vd','~',names.w[1],'*',names.w[2],' | sujeto',sep='')}

#     if(grafica[1] & n.intra <=2) {
# 		try(detach(package:lmerTest),silent=TRUE)
# 		print(graficos.lmlist.fnc(dat.st,mod.lmer0,mod.lis))
# 		try(require(lmerTest), quietly=TRUE)
# 	}

    try(assign('dat.st',dat.st,envir=.GlobalEnv),silent=TRUE) 
    #-------------------------------------------------------------------------------
    # SI RANDOM BY SUJETOS E ITEMS
    #-------------------------------------------------------------------------------
    if(random.sujeto & random.item){
    	if(is.na(covariante[1])){
			if(n.factores==1) modelo2=paste('vd','~',factores[1],'+',sep='')
			if(n.factores==2) modelo2=paste('vd','~',factores[1],'*',factores[2],'+',sep='')
			if(n.factores==3) modelo2=paste('vd','~',factores[1],'*',factores[2],'*',
	      		factores[3],'+',sep='')
			if(n.factores==4) modelo2=paste('vd','~',factores[1],'*',factores[2],'*',
	      		factores[3],'*',factores[4],'+',sep='')
			# Random
			rand0='(1|sujeto)+(1|item)';
			modelo00.b=paste('vd','~ 1 + ',rand0,sep='')
			modelo0.b=paste(modelo2,rand0,sep='')
			if(!is.na(fac.random[1])){
	  			rand1=paste('(',fac.random[1],'|sujeto)+(1|item)',sep='')
	  			modelo2=paste(modelo2,rand1,sep='')
	 		}
     	}else{
			if(n.factores==1) modelo2=paste('vd','~',covariante,'+',factores[1],'+',sep='')
			if(n.factores==2) modelo2=paste('vd','~',covariante,'+',factores[1],'*',factores[2],'+',sep='')
			if(n.factores==3) modelo2=paste('vd','~',covariante,'+',factores[1],'*',factores[2],'*',
	      		factores[3],'+',sep='')
			if(n.factores==4) modelo2=paste('vd','~',covariante,'+',factores[1],'*',factores[2],'*',
	      		factores[3],'*',factores[4],'+',sep='')
			if(is.na(fac.random[1])){
	 			rand='(1|sujeto)+ (1|item)'; rand0=rand
			}else{
	  			rand=paste('(',fac.random[1],'|sujeto)+(1|item)',sep='')
				rand0='(1|sujeto)+ (1|item)'
			}
			modelo2a=modelo2
      		modelo2=paste(modelo2,rand,sep='')
			# Random
			rand0='(1|sujeto)+(1|item)';
			modelo00.b=paste('vd','~ 1 + ',rand0,sep='')
			modelo0.b=paste(modelo2a,rand0,sep='')
     	 } # FIN DE COVARIANTE en random sujeto e item

    	# ESTIMA MODELO LMER PARA RANDOM SUJETO ITEM Y FACTOR EN SUJETO
    	if(!accuracy){
		mod.lmer00.b=lmer(eval(parse(text=modelo00.b)),data=dat.st)
		mod.lmer0.b=lmer(eval(parse(text=modelo0.b)),data=dat.st)
	}else{
		mod.lmer00.b=glmer(eval(parse(text=modelo00.b)),data=dat.st,
			family=binomial(link='logit'))
		mod.lmer0.b=glmer(eval(parse(text=modelo0.b)),data=dat.st,
			family=binomial(link='logit'))
	}
	
    	if(!is.na(fac.random[1])){
			if(!accuracy){
				mod.lmer2=lmer(eval(parse(text=modelo2)),data=dat.st)
			}else{			
				mod.lmer2=glmer(eval(parse(text=modelo2)),data=dat.st,
					family=binomial(link='logit'))
			}
		}
    } # FIN DE RANDOM SUJETOS E ITEMS
    #-------------------------------------------------------------------------------

	if(random.sujeto & !random.item & is.na(fac.random[1])){
		modelo.propuesto=modelo0;
		modelo.alternativo=modelo0 }
	if(random.sujeto & !random.item & !is.na(fac.random[1])){
		modelo.propuesto=modelo1
		modelo.alternativo=modelo0 }
	if(random.sujeto & random.item & is.na(fac.random[1])){
		modelo.propuesto=modelo0.b
		modelo.alternativo=modelo0}
	if(random.sujeto & random.item & !is.na(fac.random[1])){
		modelo.propuesto=modelo2
		modelo.alternativo=modelo0.b}

    #-------------------------------------------------------------------------------
    # SALIDAS SOLO PARA MODELOS RANDOM EN SUJETOS
    #-------------------------------------------------------------------------------
    if(random.sujeto & !random.item){
		salida=summary(mod.lmer0)
		ic=calcula.intraclass.fnc(mod.lmer00,random.sujeto,random.item)

		cat('---------------------------------------------------------------------',fill=TRUE)
		cat('***          MODELO INCONDICIONADO RANDOM SUJETOS            ***',fill=TRUE)
		print(mod.lmer00)
		cat('----------------------------------------------------------------',fill=TRUE)

		X11( ); print(qqPlot(ranef(mod.lmer0)$sujeto, main='Random Bo sujeto'))
		if(tipo==2){
			if(!accuracy){
				res.anova=anova(mod.lmer0)
			}else{
				res.anova=Anova(mod.lmer0, type=2)
			}
		}
		if(tipo==3){
			if(!accuracy){		
				res.anova=anova(mod.lmer0)
			}else{
				res.anova=Anova(mod.lmer0, type=3)
			}
		}					
				
		dat_0=cbind(na.omit(dat.st[,c('sujeto','vd')]),fitted(mod.lmer0))
     		cor.suj=t(sapply(split(dat_0,dat_0$sujeto), function(x) cor(x[,2:3])))[,2]
     		corre=t.test(cor.suj)
		correla=c(r=as.numeric(corre$estimate),eta2=as.numeric(corre$estimate^2),corre$statistic, 
				corre$parameter, p=corre$p.value)
		correla=round(correla,3)
		lista=list(modelo.propuesto=modelo.propuesto,modelo.alternativo=modelo.alternativo,
			estimado=summary(mod.lmer0),intraclass=ic,eta.2=correla,Anova=res.anova)
			
		if(!is.na(poshoc[1]) & is.na(fac.random[1]) & !accuracy){
			n.prev=length(lista); names.prev=names(lista); n.poshoc=length(poshoc)
			lista.poshoc=list()
			for(i in 1:length(poshoc))
				lista.poshoc[[i]]=difflsmeans(mod.lmer0, test.effs=poshoc[i], method.grad="simple")	
			names(lista.poshoc)=paste('poshoc: ',poshoc,sep='')
			lista[(n.prev+1):(n.prev+n.poshoc)]=lista.poshoc
			names(lista)=c(names.prev,names(lista.poshoc))
		}			
			
		# SI FAC RANDOM
		if(!is.na(fac.random[1])){
			aov.0.1=anova(mod.lmer0,mod.lmer1)
	  		row.names(aov.0.1)=c(modelo00,modelo1)
	  		chivato=unlist(aov.0.1["Pr(>Chisq)"])[2]
	  		if(chivato > 0.05){
	    		print(round(t(as.matrix(aov.0.1)),5))
	    		cat(' ',fill=TRUE)
	    		cat('*** La pendiende especificada como aleatoria:',fac.random,'no es significativa ***',fill=TRUE)
	    		cat('*** se utilizara el modelo alternativo ',fill=TRUE)
	    		cat(' ',fill=TRUE)
				if(!is.na(poshoc[1]) & !accuracy){
					n.prev=length(lista); names.prev=names(lista); n.poshoc=length(poshoc)
					lista.poshoc=list()
					for(i in 1:length(poshoc))
						lista.poshoc[[i]]=difflsmeans(mod.lmer0.b, test.effs=poshoc[i], 
							method.grad="simple")	
					names(lista.poshoc)=paste('poshoc: ',poshoc,sep='')
					lista[(n.prev+1):(n.prev+n.poshoc)]=lista.poshoc
					names(lista)=c(names.prev,names(lista.poshoc))
				}	  	    		
	  	}else{
	  		salida=summary(mod.lmer1)
			if(tipo==2){
				if(!accuracy){
					res.anova=anova(mod.lmer1)
				}else{
					res.anova=Anova(mod.lmer1, type=2)
				}
			}
			if(tipo==3){
				if(!accuracy){		
					res.anova=anova(mod.lmer1)
				}else{
					res.anova=Anova(mod.lmer1, type=3)
				}
			}		

			dat_1=cbind(na.omit(dat.st[,c('sujeto','vd')]),fitted(mod.lmer1))
			cor.suj=t(sapply(split(dat_1,dat_1$sujeto), function(x) cor(x[,2:3])))[,2]
			corre=t.test(cor.suj)
			correla=c(r=as.numeric(corre$estimate),eta2=as.numeric(corre$estimate^2),corre$statistic, 
	    		corre$parameter, p=corre$p.value)
			correla=round(correla,3)
            lista1=list(modelo.random.pend=modelo1,anova.0.1=t(round(aov.0.1,5)),
				estimado=summary(mod.lmer1),eta.2=correla, Anova=res.anova)
			lista=c(lista,lista1)
			if(!is.na(poshoc[1]) & !accuracy){
				n.prev=length(lista); names.prev=names(lista); n.poshoc=length(poshoc)
				lista.poshoc=list()
				for(i in 1:length(poshoc))
					lista.poshoc[[i]]=difflsmeans(mod.lmer1, test.effs=poshoc[i], 
						method.grad="simple")	
				names(lista.poshoc)=paste('poshoc: ',poshoc,sep='')
				lista[(n.prev+1):(n.prev+n.poshoc)]=lista.poshoc
				names(lista)=c(names.prev,names(lista.poshoc))
			}	    	
	  	}
	} # Cierra si fac.random
    } # CIERRA SOLO RANDOM EN SUJETOS
    #-------------------------------------------------------------------------------

    #-------------------------------------------------------------------------------
    # SALIDAS MODELOS RANDOM EN SUJETOS E ITEMS
    #-------------------------------------------------------------------------------

   # print(data.frame(label=c('modelo0','modelo0.b','modelo00','modelo00.b','modelo1','modelo2'),
   # modelo=c(modelo0,modelo0.b,modelo00,modelo00.b,modelo1,modelo2)))

    if(random.sujeto & random.item){
		ic=calcula.intraclass.fnc(mod.lmer00.b,random.sujeto,random.item)
		cat('---------------------------------------------------------------------',fill=TRUE)
		cat('***          MODELO INCONDICIONADO RANDOM SUJETOS E ITEMS         ***',fill=TRUE)
		cat('',fill=TRUE)
		#detach(package:lmerTest)
		print(mod.lmer00.b)
		cat('---------------------------------------------------------------------',fill=TRUE)

		X11( )
		par(mfrow=c(1,2))
		print(qqPlot(ranef(mod.lmer0.b)$sujeto,main='Random sujetos'))
 		print(qqPlot(ranef(mod.lmer0.b)$item,main='Random items'))
		par(mfrow=c(1,1))
		if(tipo==2){
			if(!accuracy) {
				res.anova=anova(mod.lmer0.b)
			}else{
				res.anova=Anova(mod.lmer0.b, type=2)
			}
		}	
		if(tipo==3){
			if(!accuracy) {
				res.anova=anova(mod.lmer0.b)
			}else{
				res.anova=Anova(mod.lmer0.b, type=3)
			}
		}
		dat_0b=cbind(na.omit(dat.st[,c('sujeto','vd')]),fitted(mod.lmer0.b))
		cor.suj=t(sapply(split(dat_0b,dat_0b$sujeto), function(x) cor(x[,2:3])))[,2]
		corre=t.test(cor.suj)
		correla=c(r=as.numeric(corre$estimate),eta2=as.numeric(corre$estimate^2),corre$statistic, corre$parameter, p=corre$p.value)
		correla=round(correla,3)
		anova.0.0b=round(anova(mod.lmer0,mod.lmer0.b),6)
		row.names(anova.0.0b)=c(modelo0,modelo0.b)
		summary(mod.lmer0.b)		
		
        lista=list(modelo.propuesto=modelo.propuesto, anova=t(anova.0.0b), 
			estimado=summary(mod.lmer0.b),intraclass=ic,eta.2=correla,Anova=res.anova)			
		if(!is.na(poshoc[1]) & is.na(fac.random[1]) & !accuracy){
			n.prev=length(lista); names.prev=names(lista); n.poshoc=length(poshoc)
			lista.poshoc=list()
			for(i in 1:length(poshoc))
				lista.poshoc[[i]]=difflsmeans(mod.lmer0.b, test.effs=poshoc[i], method.grad="simple")	
			names(lista.poshoc)=paste('poshoc: ',poshoc,sep='')
			lista[(n.prev+1):(n.prev+n.poshoc)]=lista.poshoc
			names(lista)=c(names.prev,names(lista.poshoc))
		}

		# SI FAC RANDOM
		if(!is.na(fac.random[1])){
			cat('***                 Procesando pendiente aleatoria                ***',fill=TRUE)
			cat('',fill=TRUE)
			aov.0b.2=anova(mod.lmer0.b,mod.lmer2)
			row.names(aov.0b.2)=c(modelo0.b,modelo2)
			chivato=unlist(aov.0b.2["Pr(>Chisq)"])[2]
			aov.0b.2=round(as.matrix(aov.0b.2),5)
			lista.0b.2=list(modelo.propuesto=modelo.propuesto,anova=t(aov.0b.2))
			if(chivato > 0.05){ #OJO SOLO EN PRUEBA
				print(lista.0b.2)
				cat(' ',fill=TRUE)
				cat('*** La pendiende especificada como aleatoria:',fac.random,'no es significativa ***',fill=TRUE)
				cat('*** se utilizara el modelo alternativo',fill=TRUE)
				cat('*** Se contrastara ahora la necesidad de estimar el modelo random cruzado para ***',fill=TRUE)
				cat('*** sujetos e items',fill=TRUE)
				cat(' ',fill=TRUE)
				if(!is.na(poshoc[1]) & !accuracy){
					n.prev=length(lista); names.prev=names(lista); n.poshoc=length(poshoc)
					lista.poshoc=list()
					for(i in 1:length(poshoc))
						lista.poshoc[[i]]=difflsmeans(mod.lmer0.b, test.effs=poshoc[i], 
							method.grad="simple")	
					names(lista.poshoc)=paste('poshoc: ',poshoc,sep='')
					lista[(n.prev+1):(n.prev+n.poshoc)]=lista.poshoc
					names(lista)=c(names.prev,names(lista.poshoc))
				}				
				
			}else{
				par(mfrow=c(2,2))
				ran.suj=ranef(mod.lmer2)[[2]][1]
				ran.pen=ranef(mod.lmer2)[[2]][2]
				print(qqPlot(ran.suj,main='Random sujetos'))
				print(qqPlot(ran.pen,main='Random pendiente'))
				print(qqPlot(ranef(mod.lmer2)$item,main='Random items'))
				dat_2=cbind(na.omit(dat.st[,c('sujeto','vd')]),fitted(mod.lmer2))
				cor.suj=t(sapply(split(dat_2,dat_2$sujeto), function(x) cor(x[,2:3])))[,2]
				corre=t.test(cor.suj)
				correla=c(r=as.numeric(corre$estimate),eta2=as.numeric(corre$estimate^2),corre$statistic, corre$parameter, p=corre$p.value)
				correla=round(correla,3)
				if(tipo==2) res.anova=anova(mod.lmer2, type=2)
				if(tipo==3) res.anova=anova(mod.lmer2, type=3)  
				lista2=list(modelo.0b=modelo0.b,modelo2=modelo2,anova=t(aov.0b.2),
					estimado=summary(mod.lmer2),eta.2=correla, Anova=res.anova)            
				lista=c(lista,lista2)
				if(!is.na(poshoc[1]) & !accuracy){
					n.prev=length(lista); names.prev=names(lista); n.poshoc=length(poshoc)
					lista.poshoc=list()
					for(i in 1:length(poshoc))
						lista.poshoc[[i]]=difflsmeans(mod.lmer0.b, test.effs=poshoc[i], 
							method.grad="simple")	
					names(lista.poshoc)=paste('poshoc: ',poshoc,sep='')
					lista[(n.prev+1):(n.prev+n.poshoc)]=lista.poshoc
					names(lista)=c(names.prev,names(lista.poshoc))
				}
			}
		}
    } # CIERRE SALIDAS RANDOM SUJETOS ITEMS
    #-------------------------------------------------------------------------------
	try(detach(package:lmerTest),silent=TRUE) 
  
	if(silente){
		return(lista) 
	}else{ 
		print(lista)
	}
 }
# -------------------------------------------------------------------------------

#---------------------------------------------------------------------------
# calcula.intraclass.fnc(mod.lmer00,random.sujeto=T,random.item=T)
#---------------------------------------------------------------------------
calcula.intraclass.fnc=function(estimado00,random.sujeto,random.item){
    # CAMINO INTRACLASS MODELO NULO sujeto
    if(!random.item){
      sink('tmp.mod.nulo.txt')
      print(estimado00)
      sink()
      raw00 = scan("tmp.mod.nulo.txt", sep="\n",what=character(), quiet=TRUE)
      for(i in 7:8){
        x=raw00[i]
        if(i==7){
          var.res = as.numeric(strsplit(x, '  *')[[1]][4])
        }else{
          var.suj = as.numeric(strsplit(x, '  *')[[1]][3])
        }
      }
      intraclass.suj=var.res/(var.res+var.suj)
    lista=list(cor.intraclass.sujeto=intraclass.suj)
    }
    if(random.item){
      sink('tmp.mod.nulo.txt')
      print(estimado00)
      sink()
      raw00 = scan("tmp.mod.nulo.txt", sep="\n",what=character(), quiet=TRUE)
      for(i in 7:9){
        x=raw00[i]
        if(i==7) var.item=as.numeric(strsplit(x, '  *')[[1]][4])
        if(i==8) var.suj=as.numeric(strsplit(x, '  *')[[1]][4])
        if(i==9) var.res = as.numeric(strsplit(x, '  *')[[1]][3])
      }
      intraclass.item=var.item/(var.item+var.suj+var.res)
      intraclass.suj=var.suj/(var.item+var.suj+var.res)
      lista=list(cor.intraclass.item=intraclass.item,cor.intraclass.sujeto=intraclass.suj)
    }
  return(lista)
 }
#---------------------------------------------------------------------------

# -------------------------------------------------------------------------------
# graficos.lmlist.fnc(dat.st,mod.lmer,mod.List)
# Funcion que es llamada dentro de mixed.model para plotear las rectas de los
# modelos lineales por sujeto
# -------------------------------------------------------------------------------
  graficos.lmlist.fnc=function(datos,mod.lmer,mod.lis){
	dat.st=datos
	require(lme4, quietly=TRUE)
      rand= lmList(eval(parse(text=mod.lis)), data=dat.st)
      coefi=coef(rand)
      ylim=c(min(dat.st$vd,na.rm=TRUE),max(dat.st$vd,na.rm=TRUE))
      n.par=dim(coefi)[2]-1
      nom=names(coefi)
      a=summary(mod.lmer)
      coefi.g=coef(a)[,1]
      if(n.par==2){
	plot(c(-2,2),ylim,type='n',main=paste('Pend. ',nom[2],sep=''))
	for (i in 1:dim(coefi)[1]) {
	  abline(coefi[i,1],coefi[i,2],lwd=1,lty=2,col=1)
	}
	abline(coefi.g[1],coefi.g[2],col='red',lwd=3)
      }
      if(n.par >=3){
	par(mfrow=c(2,2))
	plot(c(-2,2),ylim,type='n',main=paste('Pend. ',nom[2],sep=''))
	for (i in 1:dim(coefi)[1]) {
	  abline(coefi[i,1],coefi[i,2],lwd=1,lty=2,col=1)
	}
	abline(coefi.g[1],coefi.g[2],col='red',lwd=3)
	plot(c(-2,2),ylim,type='n',main=paste('Pend. ',nom[3],sep=''))
	for (i in 1:dim(coefi)[1]) {
	  abline(coefi[i,1],coefi[i,3],lwd=1,lty=2,col=1)
	}
	abline(coefi.g[1],coefi.g[3],col='blue',lwd=3)
	plot(c(-2,2),ylim,type='n',main=paste('Pend. ',nom[4],sep=''))
	for (i in 1:dim(coefi)[1]) {
	  abline(coefi[i,1],coefi[i,4],lwd=1,lty=2,col=1)
	}
	abline(coefi.g[1],coefi.g[4],col='green',lwd=3)
      }
  par(mfrow=c(1,1))
  try(detach(package:lme4),silent=TRUE)
  }
# -------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
# apila.los.datos.fnc(datos,fac.intra=NA, fac.inter=NA, covariante=NA,
#					col.empieza.mr=NA, col.empieza.item=NA, n.item=NA)
# -------------------------------------------------------------------------------
 apila.los.datos.fnc=function(datos=NA, fac.intra=NA, fac.inter=NA, covariante=NA,
					col.empieza.mr=NA, col.empieza.item=NA, vd=NA ,n.item=NA,
					item.repetidos=FALSE, silente=NA){

	if(class(try(is.na(datos)))=='logical') {
		cat('',fill=TRUE)
		crea.cat.fnc('apila.los.datos.fnc')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(" #Declaramos la estructura del design de OBrienKaiser 			",fill=TRUE)
		cat(" head(OBrienKaiser)										",fill=TRUE)
		cat(" fac.inter= c('treatment','gender') 			",fill=TRUE)
		cat(" fac.intra= list(fase = c('antes','despues','seguimiento'), 		",fill=TRUE)
		cat("         		 hora= 1:5 )								",fill=TRUE)
		cat(" datos.ap=apila.los.datos.fnc(OBrienKaiser, fac.inter=fac.inter, 	",fill=TRUE)
		cat(" 		fac.intra=fac.intra, col.empieza.mr=3)				",fill=TRUE)
		cat(" head(datos.ap)											",fill=TRUE)
		cat(" ",fill=TRUE)
		cat(" #DATOS CON ITEMS							 			",fill=TRUE)
		cat(" #Creamos unos datos ficticios de 100 sujetos en 20 items  		",fill=TRUE)
		cat("  datos=data.frame(mvrnorm(20,rep(0,20),Sigma=diag(1,20)))			",fill=TRUE)
		cat(" # Asignamos item como nombre de cada columna					",fill=TRUE)
		cat("  datos=crea.nombre.item.fnc(datos)           					",fill=TRUE)
		cat(" # Declaramos la estructura intra del design					",fill=TRUE)
		cat("  fac.intra=list(A=c('a1','a2'), B=c('b1','b2'))				",fill=TRUE)
		cat(" # Apilamos los datos										",fill=TRUE)
		cat("  datos.ap=apila.los.datos.fnc(datos, fac.intra=fac.intra,			",fill=TRUE)
		cat("         col.empieza.item=1, n.item=20)						",fill=TRUE)
		cat(" head(datos.ap)											",fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' En los design de medidas repetidas, es de gran utilidad que todas  ', fill=TRUE)
		cat(' las medidas realizadas a traves de las JxK.. condiciones MR se 	', fill=TRUE)
		cat(' encuentren apiladas en una sola columna de nombre vd. Esta funcion	', fill=TRUE)
		cat(' coloca todas los valores de la vd en una sola columna y genera nuevas', fill=TRUE)
		cat(' variables de localizacion del valor: sujeto, item, factores, etc.	', fill=TRUE)
		cat(' El apilado de los datos es obligatorio para los analisis mixed asi ', fill=TRUE)
		cat(' como para los Anova F1F2 y minF.							', fill=TRUE)
		cat(" ",fill=TRUE)
		cat("sites.google.com/site/ullrtoolbox/lectura-de-archivos/apila-los-datos-fnc", fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		print(argumentos('apila.los.datos.fnc'))
	return('help')
	}

	check.fac.intra.fnc(fac.intra)
	
	nombres=names(datos)
	if(is.na(silente[1])) check.factores.fnc(fac.inter,fac.intra)

	if(!is.na(fac.intra[1]) & is.na(col.empieza.mr[1]) & is.na(silente) &
			is.na(col.empieza.item[1])){
		cat('',fill=TRUE)
		cat('*** Error. Dispones de medidas repetidas y no has incluido el argumento:',fill=TRUE)
		cat('*** col.empieza.mr donde indicas en que columna empiezan las medidas repetidas.',fill=TRUE)
		cat('*** Ej: col.empieza.mr=3 ',fill=TRUE)
		cat('',fill=TRUE)
		stop( )
	}

	# CHECK SI EXISTEN LOS FACTORES INTER
      if(!is.na(fac.inter[1]) & is.na(silente)) {
		n.inter=length(fac.inter)
		ok.fac.inter=check.fac.inter(datos, fac.inter)
		if(length(ok.fac.inter)!=0)
			stop('*** No existe algun o algunos de los factores inter incluidos ***')
	 }else{
		n.inter=0
	 }
	# FIN CHECK SI EXISTEN LOS FACTORES INTER
      if(!is.na(fac.inter[1])) {
	 chivato.fac=is.factor(datos[,fac.inter])
	 if(sum(chivato.fac)!=length(fac.inter) )
	 	for(i in 1:length(fac.inter)) datos[,fac.inter[i]]=factor(datos[,fac.inter[i]])
      }

	# APILA COMPLETAMENTE ALEATORIZADO
 	if(is.na(fac.intra[1]) & is.na(n.item)){
		if(is.na(vd[1])){
		 cat('*** Tienes un design Completamente aleatorizado ***',fill=TRUE)
		 cat('*** y no has indicado la variable dependiente 	***',fill=TRUE)
		 stop( )
		}else{
		 datos$sujeto=paste('suj',1:dim(datos)[1],sep='')
		 if(length(fac.inter)==1){
			if(dim(datos)[2] > 2){
			 cat('*** Tus datos tienen mas de dos variables ***',fill=TRUE); stop( )}
		 	tabla=with(datos, aggregate(datos[,vd],
				list(sujeto,eval(parse(text=fac.inter[1]))),
					function(x) mean(x,na.rm=TRUE)))

			names(tabla)=c('sujeto',fac.inter[1],'vd')
			tabla$condicion=datos[,fac.inter[1]]
		 return(tabla)
		 }
		 if(length(fac.inter)==2){
		 	tabla=with(datos, aggregate(datos[,vd],
				list(sujeto,eval(parse(text=fac.inter[1])),
					eval(parse(text=fac.inter[2]))),
					function(x) mean(x,na.rm=TRUE)))
			names(tabla)=c('sujeto',fac.inter[1],fac.inter[2],'vd')
			tabla$condicion=paste(datos[,fac.inter[1]],datos[,fac.inter[2]],sep='.')
		 return(tabla)
		 }
		}
 	} # FIN APILA COMPLETAMENTE ALEATORIZADO

	if(is.na(col.empieza.mr[1]) & is.na(col.empieza.item[1]) & !is.na(n.item[1])){
		cat('',fill=TRUE)
		cat('*** Error. No has indicado el argumento col.empieza.item.     ***',fill=TRUE)
		cat('*** Dado que tienes items en la base de datos debes incluirlo ***',fill=TRUE)
		cat('*** en la llamada a la funcion. Ej. col.empieza.item=5        ***',fill=TRUE)
		cat('',fill=TRUE)
		stop( )
	}

 	if(!is.na(col.empieza.item[1]) & is.na(n.item[1])) {
		cat('',fill=TRUE)
		cat('*** Error. No has indicado el numero de items. Debes  ***',fill=TRUE)
		cat('*** incluir el argumento n.item en la funcion.        ***',fill=TRUE)
		cat('*** Ej.  n.item=80                                    ***',fill=TRUE)
		cat('',fill=TRUE)
		stop( )
	}

 	if(is.na(col.empieza.item[1]) & !is.na(n.item[1]) & !is.na(col.empieza.mr[1])) {
		cat('',fill=TRUE)
		cat('*** Error. Dices tener items pero no has incluido el ***',fill=TRUE)
		cat('*** argumento col.empieza.item  en la funcion.       ***',fill=TRUE)
		cat('*** Debes eliminar el argumento col.empieza.mr       ***',fill=TRUE)
		cat('*** Ej.  col.empieza.item=3                          ***',fill=TRUE)
		cat('',fill=TRUE)
		stop( )
	}

 	if(is.na(col.empieza.item[1]) & !is.na(n.item[1])) {
		cat('',fill=TRUE)
		cat('*** Error. Dices tener items pero no has incluido el ***',fill=TRUE)
		cat('*** argumento col.empieza.item  en la funcion.       ***',fill=TRUE)
		cat('*** Ej.  col.empieza.item=3                          ***',fill=TRUE)
		cat('',fill=TRUE)
		stop( )
	}

 	if(!is.na(col.empieza.item[1]) & !is.na(n.item[1])) {
	    #hay.item=strsplit(names(datos)[col.empieza.item],col.empieza.item)[[1]]
	    hay.item=strsplit(names(datos)[col.empieza.item],1)[[1]]
	    chivato=match('item',hay.item)
	    if(is.na(chivato[1])){
		cat('',fill=TRUE)
		cat('*** Error. Dices tener items entre tus variables pero no tienen ese nombre.   ***',fill=TRUE)
		cat('*** Utiliza la funcion crea.nombre.item.fnc para conseguir que tus items      ***',fill=TRUE)
		cat('*** reciban ese nombre: item1 item2 ... item100                               ***',fill=TRUE)
		cat('*** Ej:                                                                       ***',fill=TRUE)
		cat('***  mis.datos=crea.nombre.item.fnc(mis.datos, col.empieza.item=2, n.item=80) ***',fill=TRUE)
		cat('*** Si tu base de datos solo tiene items basta con llamar a la funcion asi:   ***',fill=TRUE)
		cat('***  mis.datos=crea.nombre.item.fnc(mis.datos)                                ***',fill=TRUE)
		cat('',fill=TRUE)
		stop( )
	    }
	}
  if(!is.na(col.empieza.item[1]) & !is.na(n.item[1])){
    if(n.item > dim(datos)[2]){
      cat('', fill=TRUE)
      cat('*** Error. El numero de items es superior al numero de columnas de la', fill=TRUE)
      cat('*** base de datos. Revisa el argumento n.item.', fill=TRUE)
      cat('', fill=TRUE)
      stop()      
    }
  }
  
	# MODELOS COMPLETAMENTE ALEATORIOS CON ITEMS
 	if(is.na(fac.intra[1]) & !is.na(n.item)) {
		n.fac.inter=length(fac.inter)
		n.sujetos=dim(datos)[1]
		dat_=datos[,col.empieza.item:(n.item+col.empieza.item)]
		dat.st=stack(dat_)
		names(dat.st)=c('vd','item')
		dat.st$sujeto=factor(paste('suj',1:n.sujetos,sep=''),
			levels=paste('suj',1:n.sujetos,sep=''))
		if(!is.na(covariante)) dat.st[,covariante]=datos[,covariante]
		if(n.fac.inter ==1) dat.st[,fac.inter[1]]=datos[,fac.inter[1]]
		if(n.fac.inter ==2) {
			dat.st[,fac.inter[1]]=datos[,fac.inter[1]]
			dat.st[,fac.inter[2]]=datos[,fac.inter[2]]
		}
		if(n.fac.inter <= 3){
			dat.st[,fac.inter[1]]=datos[,fac.inter[1]]
			dat.st[,fac.inter[2]]=datos[,fac.inter[2]]
			dat.st[,fac.inter[3]]=datos[,fac.inter[3]]
		}
	}

	# MODELOS MR ALEATORIOS SIN y con ITEMS
	if(!is.na(fac.intra[1])) {
			idata=crea.idata.fnc(fac.intra)
			nfac.mr=dim(idata)[2];
			fac.mr=names(idata)
			ncon.mr=dim(idata)[1];
		# CONTROLA EL STACK SEGUN HAYA O NO ITEMS EN LAS MR
		if(is.na(n.item)){
			datos.mr=datos[,col.empieza.mr:((col.empieza.mr+ncon.mr)-1)]
			n=dim(datos.mr)[1]
			dat.st=stack(datos.mr)
			names(dat.st)=c('vd','condi')
			dat.st$sujeto=factor(rep(paste('suj',1:n,sep=''),ncon.mr),
					levels=paste('suj',1:n,sep=''))
		}else{
			n.itc=n.item/ncon.mr
			datos.mr=datos[,col.empieza.item:((col.empieza.item+(n.itc*ncon.mr))-1)]
			n=dim(datos.mr)[1]
			dat.st=stack(datos.mr)
			names(dat.st)=c('vd','item')
			if(item.repetidos){
		          item=do.call(rbind,strsplit(as.character(dat.st$item),'[:.:]'))[,1]
			}else{
			  item=dat.st$item
		        }
			n.item_=length(unique(item))
			dat.st$item=factor(item, levels=paste('item',1:n.item_,sep=''))
			dat.st$sujeto=factor(paste('suj',1:n,sep=''),
				levels=paste('suj',1:n,sep=''))
		} # fin de control de presencia de items.

		# CONTROLA EL NUMERO DE FACTORES DE MEDIDAS REPETIDAS
		if (nfac.mr==1){
			nlev.A=nlevels(idata[,1])
			if(is.na(n.item)){
    				names(dat.st)=c('vd',fac.mr,'sujeto')
				dat.st[,fac.mr[1]]=factor(rep(levels(idata[,1]),each=n),
					levels=fac.intra[[1]])
			}else{
    				names(dat.st)=c('vd','item','sujeto')
				dat.st[,fac.mr[1]]=factor(rep(levels(idata[,1]),each=n.itc*n),
					levels=fac.intra[[1]])
			}
		} # FIN 1 MR
		if (nfac.mr==2){
			nlev.A=nlevels(idata[,1])
			nlev.B=nlevels(idata[,2])
			if(is.na(n.item)){
				dat.st[,fac.mr[1]]=factor(rep(levels(idata[,1]),each=n*nlev.B),
					levels=fac.intra[[1]])
				dat.st[,fac.mr[2]]=factor(rep(rep(levels(idata[,2]),each=n),nlev.A),
					levels=fac.intra[[2]])
				dat.st=dat.st[,-2]
			}else{
				dat.st[,fac.mr[1]]=factor(rep(levels(idata[,1]),each=n.itc*n*nlev.B),
					levels=fac.intra[[1]])
				dat.st[,fac.mr[2]]=factor(rep(rep(levels(idata[,2]),each=n.itc*n),nlev.A),
					levels=fac.intra[[2]])
			}
		} # FIN 2 MR
		if (nfac.mr==3){
			nlev.A=nlevels(idata[,1])
			nlev.B=nlevels(idata[,2])
			nlev.C=nlevels(idata[,3])
			if(is.na(n.item)){
				dat.st=dat.st[order(dat.st$sujeto),]
				idata.j= do.call(paste,idata)
				idata.j=data.frame(do.call(rbind,strsplit(idata.j,' ')))
				names(idata.j)=names(fac.intra)
				dat.st=cbind(dat.st,idata.j)
				dat.st=dat.st[,-2]
				for(k in 1:nfac.mr)
				  dat.st=reordena.factor.fnc(dat.st,names(fac.intra)[k],fac.intra[[k]],silente=TRUE)
			}else{
				dat.st[,fac.mr[1]]=factor(rep(levels(idata[,1]),each=(n.item*n)/nlev.A),
					levels=fac.intra[[1]])
				dat.st[,fac.mr[2]]=factor(rep(rep(levels(idata[,2]),each=n.itc*n*nlev.C),nlev.A),
					levels=fac.intra[[2]])
				dat.st[,fac.mr[3]]=factor(rep(rep(levels(idata[,3]),each=n.itc*n),nlev.A*nlev.B),
					levels=fac.intra[[3]])
			}
		} # FIN 3 MR

		# CONTROLA SI ES SPLIT PLOT (FACTORES INTER)
		if(!is.na(fac.inter[1])) {
			n.fac.inter=length(fac.inter)
			if(n.fac.inter ==1) { dat.st[,fac.inter[1]]=datos[,fac.inter[1]]}
			if(n.fac.inter ==2) {
				dat.st[,fac.inter[1]]=datos[,fac.inter[1]]
				dat.st[,fac.inter[2]]=datos[,fac.inter[2]]
			}
			if(n.fac.inter ==3){
				dat.st[,fac.inter[1]]=datos[,fac.inter[1]]
				dat.st[,fac.inter[2]]=datos[,fac.inter[2]]
				dat.st[,fac.inter[3]]=datos[,fac.inter[3]]
			}
		}
	}
	# FIN MODELOS MR ALEATORIOS SIN y con ITEMS

 	dat.st=crea.condicion.fnc(dat.st,fac.intra,fac.inter,silente=TRUE)
	# SI COVARIANTES
	if(!is.na(covariante[1])){
	  if(is.numeric(covariante[1])) covariante=nombres[covariante]
	  datos$sujeto=paste('suj',1:dim(datos)[1],sep='')
	  dat.st=merge(dat.st,datos[,c('sujeto',covariante)],by='sujeto',
		all.x=TRUE, all.y=TRUE)
	}
	# FIN SI COVARIANTES

	if(is.na(silente[1])) {
	  crea.cat.fnc('APILADO DE LOS DATOS')
	  cat('*** Esta es la cabecera de los datos apilados: ***',
		fill=TRUE)
	  cat('',fill=TRUE)
	  print(head(dat.st))
	}
 return(dat.st)
 } # FIN DE RUTINA
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# crea.condicion.fnc(dat.st,fac.intra,fac.inter)
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
# crea.condicion.fnc(dat.st,fac.intra,fac.inter)
#---------------------------------------------------------------------------
 crea.condicion.fnc=function(datos,fac.intra=NA,fac.inter=NA, silente=NA){
	dat.st=datos

 	if(is.na(silente[1])) check.factores.fnc(fac.inter,fac.intra)
	check.fac.intra.fnc(fac.intra)
	
	# CHECK SI EXISTEN LOS FACTORES INTER
	if(!is.na(fac.inter[1])) {
		n.inter=length(fac.inter)
		ok.fac.inter=check.fac.inter(datos, fac.inter)
		if(length(ok.fac.inter)!=0)
			stop('*** No existe algun o algunos de los factores inter incluidos ***')
	 }else{
		n.inter=0
	 }
	# FIN SI EXISTEN LOS FACTORES INTER

	 # DEFINE MATRIZ PARA SOLO INTERGRUPO
	 if(!is.na(fac.inter[1]) & is.na(fac.intra[1])){
	    n.fac.inter=length(fac.inter); 
	    lista=list( )
	    for(i in 1:length(fac.inter)) lista[[i]]=is.factor(dat.st[,fac.inter[i]])
	    indice=as.logical(do.call(rbind,lista))
	    
	    if(sum(indice)!= n.fac.inter){
	    	if(sum(indice)==0){
				for(i in 1:length(fac.inter))
					dat.st[,fac.inter[i]]=as.factor(dat.st[,fac.inter[i]])	    	
	    	}else{
	    	   	fac.inter_=fac.inter[!indice]
	    		for(i in length(fac.inter_))
					dat.st[,fac.inter_[i]]=as.factor(dat.st[,fac.inter_[i]])
			}
		}		
		
		if(n.fac.inter==1) niveles=levels(dat.st[,fac.inter[1]])
	    if(n.fac.inter==2){
	      exp1=expand.grid(f1=levels(dat.st[,fac.inter[2]]),
		        f2=levels(dat.st[,fac.inter[1]]))
	      niveles=paste(exp1$f2,exp1$f1,sep='.')
	    }  
	    if(n.fac.inter==3){ 
	      exp1=expand.grid(f1=levels(dat.st[,fac.inter[3]]),
	        f2=levels(dat.st[,fac.inter[2]]),f3=levels(dat.st[,fac.inter[1]]))
	      niveles=paste(exp1$f3,exp1$f2,exp1$f1,sep='.')	      
	    }  
	    if(n.fac.inter==4){ 
	      exp1=expand.grid(f1=levels(dat.st[,fac.inter[4]]),
	        f2=levels(dat.st[,fac.inter[3]]),f3=levels(dat.st[,fac.inter[2]]),
	        f4=levels(dat.st[,fac.inter[1]]))
	      niveles=paste(exp1$f4,exp1$f3,exp1$f2,exp1$f1,sep='.')	      
	    }  
	    indice.caso.completo=complete.cases(dat.st[,fac.inter])	    
	}
	# FIN DE MATRIX DE NIVELES PARA INTERGRUPO
	
	# DEFINE MATRIZ PARA SOLO INTRAGRUPO
	if(!is.na(fac.intra[1]) & is.na(fac.inter[1])){
	    n.fac.intra=length(names(fac.intra))
	    names.f.w=names(fac.intra)
	    lista=list( )
	    for(i in 1:length(names.f.w)) lista[[i]]=is.factor(dat.st[,names.f.w[i]])
	    indice=as.logical(do.call(rbind,lista))

	    if(sum(indice)!= n.fac.intra){
	    	if(sum(indice)==0){
	    		for(i in 1:n.fac.intra)
	      			dat.st[,names.f.w[i]]=as.factor(dat.st[,names.f.w[i]])
	    	}else{	
	         	names.f.w_=names.f.w[!indice]
	         	for(i in 1:length(names.f.w_))
	      			dat.st[,names.f.w_[i]]=as.factor(dat.st[,names.f.w_[i]])
	      	}
	    }		
	    
	    if(n.fac.intra==1) niveles=levels(dat.st[,names.f.w[1]])  
	    if(n.fac.intra==2){
	      exp1=expand.grid(f1=levels(dat.st[,names.f.w[2]]),
		      f2=levels(dat.st[,names.f.w[1]]))
	      niveles=paste(exp1$f2,exp1$f1,sep='.')
	    }
	    if(n.fac.intra==3){ 
	      exp1=expand.grid(f1=levels(dat.st[,names.f.w[3]]),
		  f2=levels(dat.st[,names.f.w[2]]),f3=levels(dat.st[,names.f.w[1]]))
	      niveles=paste(exp1$f3,exp1$f2,exp1$f1,sep='.')	      
	    }  
	    indice.caso.completo=complete.cases(dat.st[,names.f.w])
	}
	# FIN MATRIZ PARA SOLO INTRAGRUPO
	
	# DEFINE MATRIZ PARA SPLIT-PLOT
	if(!is.na(fac.intra[1]) & !is.na(fac.inter[1])){
	    list.fac=c(fac.inter,names(fac.intra))
	    n.factores=length(list.fac)
	    lista=list()
	    for(i in 1:length(list.fac)) lista[[i]]=is.factor(dat.st[,list.fac[i]])
	    indice=as.logical(do.call(rbind,lista))
	    if(sum(indice)!= n.factores){
	    	if(sum(indice)==0){
	    		for(i in 1:n.factores)
	      			dat.st[,list.fac[i]]=as.factor(dat.st[,list.fac[i]])
	    	}else{	
	    		list.fac_=list.fac[!indice]
	    		for(i in 1:length(list.fac_))
	      			dat.st[,list.fac_[i]]=as.factor(dat.st[,list.fac_[i]])
	      	}		
		}	    

		if(n.factores==1) niveles=levels(dat.st[,list.fac[1]])
	    if(n.factores==2){
	      exp1=expand.grid(f1=levels(dat.st[,list.fac[2]]),
		  f2=levels(dat.st[,list.fac[1]]))
	      niveles=paste(exp1$f2,exp1$f1,sep='.')
	    }  
	    if(n.factores==3){
	      exp1=expand.grid(f1=levels(dat.st[,list.fac[3]]),
		    f2=levels(dat.st[,list.fac[2]]),
		    f3=levels(dat.st[,list.fac[1]]))
	      niveles=paste(exp1$f3,exp1$f2,exp1$f1,sep='.')	    
	    }
	    if(n.factores==4){
	      exp1=expand.grid(f1=levels(dat.st[,list.fac[4]]),
	                       f2=levels(dat.st[,list.fac[3]]),
				f3=levels(dat.st[,list.fac[2]]),
	                       f4=levels(dat.st[,list.fac[1]])
                         )
	      niveles=paste(exp1$f4,exp1$f3,exp1$f2,exp1$f1,sep='.')	    
	    }      
	    if(n.factores==5){
	      exp1=expand.grid(f1=levels(dat.st[,list.fac[5]]),
	                       f2=levels(dat.st[,list.fac[4]]),
				f3=levels(dat.st[,list.fac[3]]),
	                       f4=levels(dat.st[,list.fac[2]]),
	                       f5=levels(dat.st[,list.fac[1]])
	                       )
	      niveles=paste(exp1$f5,exp1$f4,exp1$f3,exp1$f2,exp1$f1,sep='.')	    
	    }      
	    if(n.factores==6){
	      exp1=expand.grid(f1=levels(dat.st[,list.fac[6]]),
	                       f2=levels(dat.st[,list.fac[5]]),
				f3=levels(dat.st[,list.fac[4]]),
	                       f4=levels(dat.st[,list.fac[3]]),
	                       f5=levels(dat.st[,list.fac[2]]),
	                       f6=levels(dat.st[,list.fac[1]])
	      )
	      niveles=paste(exp1$f6,exp1$f5,exp1$f4,exp1$f3,exp1$f2,exp1$f1,sep='.')	    
	    }      	    
	indice.caso.completo=complete.cases(dat.st[,list.fac])
	}
	# FIN MATRIZ PARA SPLIT-PLOT
	    
	if(is.na(fac.intra[1])){
	  dat.st$reg__=1:dim(dat.st)[1]; datos.ori=dat.st; 
	  dat.st=dat.st[indice.caso.completo,]     
		n.fac.inter=length(fac.inter)
		if(n.fac.inter==1) condi=dat.st[,fac.inter]
		if(n.fac.inter==2) condi=paste(dat.st[,fac.inter[1]],dat.st[,fac.inter[2]],sep='.')
		if(n.fac.inter==3) condi=paste(dat.st[,fac.inter[1]],dat.st[,fac.inter[2]],
			dat.st[,fac.inter[3]],sep='.')
		if(n.fac.inter==4) condi=paste(dat.st[,fac.inter[1]],dat.st[,fac.inter[2]],
			dat.st[,fac.inter[3]],dat.st[,fac.inter[4]],sep='.')
	}
	if(!is.na(fac.intra[1]) & is.na(fac.inter[1])){
	  dat.st$reg__=1:dim(dat.st)[1]; datos.ori=dat.st; 
	  dat.st=dat.st[indice.caso.completo,]     
	  n.fac.intra=length(names(fac.intra))
		names.f.w=names(fac.intra)
		if(n.fac.intra==1) condi=dat.st[,names.f.w]
		if(n.fac.intra==2) condi=paste(dat.st[,names.f.w[1] ],
						dat.st[,names.f.w[2]],sep='.')
		if(n.fac.intra==3) condi=paste(dat.st[,names.f.w[1]],
					dat.st[,names.f.w[2]],dat.st[,names.f.w[3]],sep='.')
	}
	if(!is.na(fac.intra[1]) & !is.na(fac.inter[1])){
	  dat.st$reg__=1:dim(dat.st)[1]; datos.ori=dat.st; 
	  dat.st=dat.st[indice.caso.completo,]       
	  n.fac.inter=length(fac.inter)
	  n.fac.intra=length(names(fac.intra))
	  names.f.w=names(fac.intra)
	  if(n.fac.inter==1) condi=paste(dat.st[,fac.inter[1]],sep='.')
	  if(n.fac.inter==2) condi=paste(dat.st[,fac.inter[1]],dat.st[,fac.inter[2]],sep='.')
	  if(n.fac.inter==3) condi=paste(dat.st[,fac.inter[1]],dat.st[,fac.inter[2]],
	      dat.st[,fac.inter[3]],sep='.')		
	  if(n.fac.intra==1) condi=paste(condi,dat.st[,names.f.w],sep='.')
	  if(n.fac.intra==2) condi=paste(condi,dat.st[,names.f.w[1] ],dat.st[,names.f.w[2]],sep='.')
	  if(n.fac.intra==3) condi=paste(condi,dat.st[,names.f.w[1]],dat.st[,names.f.w[2]],
		dat.st[,names.f.w[3]],sep='.')
	}
 	dat.st$condicion=factor(condi)
 	dat.st=reordena.factor.fnc(dat.st, que.factor='condicion', niveles=niveles, 
	  silente=TRUE, hacer.NA=TRUE)
	  
  if(dim(dat.st)[1]!=dim(datos.ori)[1]){
    resto=datos.ori[!indice.caso.completo,]
    resto$condicion=NA
    dat.st=rbind(dat.st,resto)
    dat.st=ordena.por.variable.fnc(dat.st, 'reg__', silente=TRUE)
    que.col=match(c('reg__','indice'),names(dat.st))
    dat.st=dat.st[,-c(que.col)]
  }else{
    que.col=match('reg__',names(dat.st))
    dat.st=dat.st[,-c(que.col)]
  }
 return(dat.st)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# Crea la matriz de J(J-1) filas y en cada fila la combinacion de los my.levels
# del factor que deben contrastarse para generar las J(J-1) contrastes par a par
# Xi=matrix(c(1:nlev.A),1,nlev.A); Xi
# tot.pares.fnc(Xi)
#---------------------------------------------------------------------------
 tot.pares.fnc=function(xI) {
	alma=NULL; A=ncol(xI)
	for (k in 1:nrow(xI)) {
      tal=xI[k,]
		for (i in 1:(A-1)){
			for (j in (i+1 : A)){
	 			alma=rbind(alma,c(tal[i],tal[j]))
			}
		}
	}
	alma=alma[complete.cases(alma),]
 return(alma);
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# Coloca las etiquetas de las J(J-1) comparaciones a partir de la matriz de
# todos los pares calculadas en la funcion anterior
#---------------------------------------------------------------------------
 etiqueta.fnc=function(factor,indi) {
 if(is.null(dim(indi))) {
	etiqueta=paste(factor[indi[1]],factor[indi[2]],sep='-')
 } else {
 	etiqueta=integer(nrow(indi))
 	for (i in 1:length(etiqueta)){
  		etiqueta[i]=paste(factor[indi[i,1]],factor[indi[i,2]],sep='-')
 	}
 }
 return(etiqueta)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#  que.efecto='treatment:gender:fase:hora'
#  tablas.descriptivos.fnc(datos.modo.inter,que.efecto)
#---------------------------------------------------------------------------
 tabla.descriptivos.fnc=function(datos,que.efecto,vd=NA){
	datos.modo.inter=datos

	if(vd=='vd') vd=NA
	nombres=names(datos.modo.inter)
	if(is.na(match('vd',nombres)) & is.na(vd[1])) {
	  cat('*** No parece que hayas introducido como argumento tus datos apilados.     ***',fill=TRUE)
	  cat('*** Si quieres conocer la tabla de medias para una variable dependiente    ***',fill=TRUE)
	  cat('*** en particular, a partir de una base de datos no apilada (solo hay      ***',fill=TRUE)
	  cat('*** factores intergrupo en tu desig, introduce  por lo tanto el            ***',fill=TRUE)
	  cat('*** argumento vd igual a la variable dependiente que deseas en la funcion. ***',fill=TRUE)
	stop( )
	}
	factores=unlist(strsplit(que.efecto,':'))
	n.fac=length(factores)
	Xi=matrix(c(1:n.fac),1,n.fac); Xi
	indi=tot.pares.fnc(Xi)
	etiquetas=etiqueta.fnc(factores,indi)
	que.tablas=strsplit(etiquetas,'-')
	tablas=list( )
        if(is.na(vd[1])){
	  for(i in 1:length(que.tablas)){
		varia=que.tablas[[i]]
		media=with(datos.modo.inter,
			tapply(vd,list(eval(parse(text=varia[1])),eval(parse(text=varia[2]))),
				function(x) mean(x,na.rm=TRUE)))
		dt=with(datos.modo.inter,
			tapply(vd,list(eval(parse(text=varia[1])),eval(parse(text=varia[2]))),
				function(x) sd(x,na.rm=TRUE)))
		n=with(datos.modo.inter,
			tapply(vd,list(eval(parse(text=varia[1])),eval(parse(text=varia[2]))),
				length))
		lista=list(media=round(media,3), dt=round(dt,3), n=round(n,3))
		tablas[[i]]=lista
	  }
	  if(n.fac==3){
		media=ftable(with(datos.modo.inter,
			tapply(vd,list(eval(parse(text=factores[1])),eval(parse(text=factores[2])),
				eval(parse(text=factores[3]))),function(x) mean(x,na.rm=TRUE))))
		dt=ftable(with(datos.modo.inter,
			tapply(vd,list(eval(parse(text=factores[1])),eval(parse(text=factores[2])),
				eval(parse(text=factores[3]))),function(x) sd(x,na.rm=TRUE))))
		n=ftable(with(datos.modo.inter,
			tapply(vd,list(eval(parse(text=factores[1])),eval(parse(text=factores[2])),
				eval(parse(text=factores[3]))),length)))
		triple=list(media=media,dt=dt,n=n)
	        names(triple)=paste(names(triple),que.efecto,sep='.')
		print(triple)
	  }
	  if(n.fac==4){
		media=ftable(with(datos.modo.inter,
			tapply(vd,list(eval(parse(text=factores[1])),eval(parse(text=factores[2])),
				eval(parse(text=factores[3])),eval(parse(text=factores[4]))),
				function(x) mean(x,na.rm=TRUE))))
		dt=ftable(with(datos.modo.inter,
			tapply(vd,list(eval(parse(text=factores[1])),eval(parse(text=factores[2])),
				eval(parse(text=factores[3])),eval(parse(text=factores[4]))),
				function(x) sd(x,na.rm=TRUE))))
		n=ftable(with(datos.modo.inter,
			tapply(vd,list(eval(parse(text=factores[1])),eval(parse(text=factores[2])),
			eval(parse(text=factores[3])),eval(parse(text=factores[4]))),length)))
		triple=list(media=media,dt=dt,n=n)
	        names(triple)=paste(names(triple),que.efecto,sep='.')
		print(triple)
	  }
	}else{
	  datos=datos.modo.inter
	  for(i in 1:length(que.tablas)){
		varia=que.tablas[[i]]
		que.vd=datos[,vd]
		media=with(datos,
			tapply(que.vd,list(eval(parse(text=varia[1])),eval(parse(text=varia[2]))),
				function(x) mean(x,na.rm=TRUE)))
		dt=with(datos,
			tapply(que.vd,list(eval(parse(text=varia[1])),eval(parse(text=varia[2]))),
				function(x) sd(x,na.rm=TRUE)))
		n=with(datos,
			tapply(que.vd,list(eval(parse(text=varia[1])),eval(parse(text=varia[2]))),
				length))
		lista=list(media=round(media,3), dt=round(dt,3), n=round(n,3))
		tablas[[i]]=lista
	  }
 	  if(n.fac==3){
		que.vd=datos[,vd]
		media=ftable(with(datos,
			tapply(que.vd,list(eval(parse(text=factores[1])),eval(parse(text=factores[2])),
				eval(parse(text=factores[3]))),function(x) mean(x,na.rm=TRUE))))
		dt=ftable(with(datos,
			tapply(que.vd,list(eval(parse(text=factores[1])),eval(parse(text=factores[2])),
				eval(parse(text=factores[3]))),function(x) sd(x,na.rm=TRUE))))
		n=ftable(with(datos,
			tapply(que.vd,list(eval(parse(text=factores[1])),eval(parse(text=factores[2])),
				eval(parse(text=factores[3]))),length)))
		triple=list(media=media,dt=dt,n=n)
	        names(triple)=paste(names(triple),que.efecto,sep='.')
		print(triple)
	  }
	  if(n.fac==4){
		media=ftable(with(datos.modo.inter,
			tapply(que.vd,list(eval(parse(text=factores[1])),eval(parse(text=factores[2])),
				eval(parse(text=factores[3])),eval(parse(text=factores[4]))),
				function(x) mean(x,na.rm=TRUE))))
		dt=ftable(with(datos.modo.inter,
			tapply(que.vd,list(eval(parse(text=factores[1])),eval(parse(text=factores[2])),
				eval(parse(text=factores[3])),eval(parse(text=factores[4]))),
				function(x) sd(x,na.rm=TRUE))))
		n=ftable(with(datos.modo.inter,
			tapply(que.vd,list(eval(parse(text=factores[1])),eval(parse(text=factores[2])),
			eval(parse(text=factores[3])),eval(parse(text=factores[4]))),length)))
		triple=list(media=media,dt=dt,n=n)
	        names(triple)=paste(names(triple),que.efecto,sep='.')
		print(triple)
	  }

	}
	names(tablas)=etiquetas
	# SI SOLO HAY UN FACTOR EXTRAE LA DIAGONAL
	if(n.fac==1){
		tablas=tablas[[1]]
		nombres=names(tablas)
		for(i in 1:length(tablas)){
	  		tab=data.frame(diag(tablas[[i]]))
	  		names(tab)=nombres[i]
	  	tablas[[i]]=tab
 		}
 	}
	tablas
 return(tablas)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# nombres.var.fnc(datos) devuelve el nombre de las variables de un
# data.frame de tal forma que puede verse facilmente el n˙mero de las var
#---------------------------------------------------------------------------
 var.names.fnc=function(datos){
	data.frame(names(datos))
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# FUNCION: efectos.simples.fnc(agregados,interaccion,hay.inter)
#	hay.inter=c(T,T)
# 	efectos.simples.fnc(datos.agregados,interaccion='frontal:grupo',
#					hay.inter)
#---------------------------------------------------------------------------
 efectos.simples.fnc=function(datos,interaccion, hay.inter=NA, fac.inter=NA,
                 fac.intra=NA,vd=NA, silente=FALSE,apilados=NA, ylab='VD', color=TRUE, 
                                     etiqueta=NA,limites=NA, titulo=NA, grafica=TRUE){
    datos.agregados=datos

    check.fac.intra.fnc(fac.intra)
    check.factores.fnc(fac.inter,fac.intra)

    if(!silente) crea.cat.fnc('CONTRASTE DE EFECTOS SIMPLES')
    dat_=datos.agregados
    facts=strsplit(interaccion,':'); fac1=facts[[1]][1]; fac2=facts[[1]][2]

    nombres=names(datos)
    hay.fac1=match(fac1,nombres); hay.fac2=match(fac2,nombres)
    if(is.na(hay.fac1[1]) | is.na(hay.fac2[1])){
      cat('',fill=TRUE)
      cat('*** Error. Revisa el nombre de la interaccion introducida:',interaccion,'es incorrecta ***',fill=TRUE)
      cat('*** Algun o algunos de los factores de la interaccion no existen en la base de datos   ***',fill=TRUE)
    stop( )
    }

    # SI NA HAY.INTER
    if(is.na(hay.inter[1])){
      es.inter1=match(fac1, fac.inter);	es.intra1=match(fac1, names(fac.intra))
      es.inter2=match(fac2, fac.inter);	es.intra2=match(fac2, names(fac.intra))
      if(is.na(es.inter1[1]) & is.na(es.inter2[1])) hay.inter=c(F,F)
      if(!is.na(es.inter1[1]) & !is.na(es.inter2[1])) hay.inter=c(T,T)
      if(!is.na(es.inter1[1]) & is.na(es.inter2[1])) hay.inter=c(T,F)
      if(is.na(es.inter1[1]) & !is.na(es.inter2[1])) hay.inter=c(F,T)
    }


	# CREAMOS LA ETIQUETA QUE.VD
	if(is.na(vd)){
 		what.vd='vd'
	}else{
		if(is.numeric(vd)){
			nombres=names(dat_)
			what.vd=nombres[vd]
			vd=what.vd
		}else{
			what.vd=vd
		}
	}
	# CIERRE ETIQUETA QUE.VD

	# SI VD NO ES 'VD' SUSTITUYE EL NOMBRE POR VD.
    if(!is.na(vd[1]) & vd!='vd') {
		que.vd=match('vd',names(dat_))
		if(!is.na(que.vd)) dat_=dat_[,-c(que.vd)]
		que.col=match(vd,names(dat_))
		nombres=names(dat_)
		nombres[que.col]='vd'
		names(dat_)=nombres
		vd='vd'
		if(is.na(apilados)){
			dat_=try(apila.los.datos.fnc(
					dat_, fac.inter=c(fac1,fac2), vd=vd),silent=TRUE)
			if(class(dat_)=='try-error') {
		  		cat('',fill=TRUE)
		  		cat('*** Error. Si tus datos ya estan apilados y has solicitado',fill=TRUE)
		  		cat('*** una variable dependiente diferente de vd incluye el   ',fill=TRUE)
		  		cat('*** argumento apilados=T                                  ',fill=TRUE)
		  	stop( )
			}
		vd=NA
		}
    }
    # FIN DE SUSTITUCION DE CUALQUIER NOMBRE DE VD POR VD

    # SI LOS AGREGADOS SON POR ITEMS SUSTITUYE ITEM POR SUJETO
    if(is.na(vd) | vd=='vd'){
    	nombres=names(dat_)
    	hay.item=match('item',nombres)
		hay.sujeto=match('sujeto',nombres)
		if(is.na(hay.sujeto) & !is.na(hay.item)) {
			nombres[hay.item]='sujeto'
			names(dat_)=nombres
		}
	}
 	# FIN SI AGREGADO POR ITEMS

    tabla.medias=with(dat_,tapply(vd,list(eval(parse(text=fac1)),
					eval(parse(text=fac2))),function(x) mean(x,na.rm=TRUE)))
    dat_[,fac1]=factor(dat_[,fac1])
    dat_[,fac2]=factor(dat_[,fac2])

    x.A=split(dat_,dat_[,fac1])
    x.B=split(dat_,dat_[,fac2])
	if(!is.na(vd) & vd=='vd') vd=NA
    if(hay.inter[1] & hay.inter[2] & is.na(vd)){
		efecto.simple.BenA=compara.pares.fnc(x.A,fac2,repetidas=FALSE)
 		efecto.simple.AenB=compara.pares.fnc(x.B,fac1,repetidas=FALSE)
    }
    if(hay.inter[1] & !hay.inter[2] & is.na(vd)){
		efecto.simple.BenA=compara.pares.fnc(x.A,fac2,repetidas=TRUE,vd=NA)
 		efecto.simple.AenB=compara.pares.fnc(x.B,fac1,repetidas=FALSE,vd=NA)
    }
    if(!hay.inter[1] & !hay.inter[2] & is.na(vd)) {
		efecto.simple.BenA=compara.pares.fnc(x.A,fac2,repetidas=TRUE,vd=NA)
 		efecto.simple.AenB=compara.pares.fnc(x.B,fac1,repetidas=TRUE,vd=NA)
     }
    if(!hay.inter[1] & hay.inter[2] & is.na(vd)) {
		efecto.simple.BenA=compara.pares.fnc(x.A,fac2,repetidas=FALSE,vd=vd)
 		efecto.simple.AenB=compara.pares.fnc(x.B,fac1,repetidas=TRUE,vd=vd)
    }
	efecto.simple.AenB=lapply(efecto.simple.AenB, function(x) {
			x$eta2=round(x$t^2/(x$t^2 + x$df),3)
			x=x[,c(1,2,4,3)]
			return(x)})
	efecto.simple.BenA=lapply(efecto.simple.BenA, function(x) {
			x$eta2=round(x$t^2/(x$t^2 + x$df),3)
			x=x[,c(1,2,4,3)]
			return(x)})

	# Hochberg tipo I correction por all simple effects
	p.1=integer()
	for (i in 1:length(efecto.simple.BenA))
		p.1=cbind(p.1,efecto.simple.BenA[[i]]$p)
	p.1=stack(as.data.frame(p.1));
	p.1$p.hochberg=p.adjust(p.1$values,method="hochberg")
	p.hochberg=split(p.1,p.1[,2])
	for (i in 1:length(efecto.simple.BenA))
		efecto.simple.BenA[[i]]$p.hocberg=p.hochberg[[i]][,3]
	p.2=integer()
	for (i in 1:length(efecto.simple.AenB))
		p.2=cbind(p.2,efecto.simple.AenB[[i]]$p)
	p.2=stack(as.data.frame(p.2));
	p.2$p.hochberg=p.adjust(p.2$values,method="hochberg")
	p.hochberg=split(p.2,p.2[,2])
	for (i in 1:length(efecto.simple.AenB))
		efecto.simple.AenB[[i]]$p.hocberg=p.hochberg[[i]][,3]
    label.1=paste('Contraste_',fac2,'_en_cada_nivel_de_',fac1,sep='')
    label.2=paste('Contraste ',fac1,'_en_cada_nivel_de_',fac2,sep='')
    efectos.simples=list(label.1,BinA=efecto.simple.BenA,label.2,AinB=efecto.simple.AenB)
    if(grafica){
	X11( )
	n.lev=nlevels(dat_[,fac1])-0.5
	maximo=max(tabla.medias,na.rm=TRUE)
	minimo=min(tabla.medias,na.rm=TRUE)
	if(is.na(limites[1])) limites=c(minimo,maximo)
	if(is.na(etiqueta[1])) etiqueta=c(n.lev,maximo)
	grafica.interaccion.fnc(dat_,interaccion, ylim=limites,ylab=ylab,color=color,
		posicion.etiqueta=etiqueta,size.font=NA,verticales=NA, titulo=titulo)
    }
 return(list(que.vd=what.vd,interaccion=interaccion ,medias=tabla.medias,efectos.simples=efectos.simples))
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# grafica.interaccion.fnc(datos.apilados,interaccion='a:b')
#---------------------------------------------------------------------------
 grafica.interaccion.fnc=function(datos,interaccion,ylim=NA, ylab=NA, color=TRUE,
		posicion.etiqueta=NA,size.font=NA,verticales=NA, pivote=NA, 
                                                     titulo=NA, vd=NA){
	if(is.na(ylab[1])) ylab='VD'
	datos.apilados=datos
	posicion=posicion.etiqueta
	if(is.na(size.font[1])) size.font=0.8
	dat_=datos.apilados

	if(is.na(vd[1])) {
	  vd='vd'
	}else{
	  nombres=names(dat_)
	  col.vd=match(vd,nombres)
	  nombres[col.vd]='vd'
	  names(dat_)=nombres
	}

  	facts=strsplit(interaccion,':');
	n.fac=length(facts[[1]])

	for(i in 1:n.fac){
	    if(!is.factor(dat_[,facts[[1]][i]]))
		  dat_[,facts[[1]][i]]=as.factor(dat_[,facts[[1]][i]])
	}
	
	# INTERACCIONES DOBLES
	if(n.fac==2){
		fac1=facts[[1]][1]; fac2=facts[[1]][2]
    		tabla=with(dat_,
			tapply(vd,list(eval(parse(text=fac1)),
			eval(parse(text=fac2))),function(x) mean(x,na.rm=TRUE)))
		etiquetas=rownames(tabla)
 		legenda=levels(dat_[,fac2])
 		if(is.null(legenda)){
		  legenda=unique(dat_[,fac2])
		  cat('', fill=TRUE)
		  cat('*** WARNING. La variable incluida como segundo termino de la interaccion no es factor',fill=TRUE)
		  cat('*** se han seleccionado los valores no repetidos de esa variable como niveles para ese',fill=TRUE)
		  cat('*** factor. Si deseas un ordenamiento distinto al alfabetico utiliza la funcion:',fill=TRUE)
		  cat('*** reorder.factor.fnc.',fill=TRUE)
		  cat("*** Ej: datos = reorder.factor.fnc(dat, which.factor='group', new.levels=c('C','B','A'))",fill=TRUE)
		} 			
 		
		minimo=round(min(tabla),1); 	minimo=minimo-0.1*minimo
		maximo=round(max(tabla),1);	maximo=maximo+0.1*maximo
		if (is.na(ylim)[1]) ylim=c(minimo,maximo)
		colores=length(unique(as.character(dat_[,fac2])))

	# SE CREA LA GRAFICA
		plot(tabla[,1],type='l',lwd=2,axes=FALSE, ann=FALSE, ylim=ylim)
 		axis(1, at=1:length(etiquetas), lab=etiquetas, cex.axis=size.font)
		axis(2)
		if(color){
		    for(i in 2:dim(tabla)[2])lines(tabla[,i],type='l',lwd=2,col=i)
		}else{
		    for(i in 2:dim(tabla)[2])lines(tabla[,i],type='l',lwd=2,lty=i)
		}
 		box()
 		title(xlab=fac1)
 		title(ylab=ylab)
		if(is.na(posicion[1])){
			n.label=length(etiquetas)
			pos1=floor((n.label*80)/100)
			pos2=ceiling((ylim[2]*80)/100)
			posicion=c(pos1,pos2)
		}
		if(is.na(titulo[1])){
 		   title(main=paste('Interaccion: ',interaccion,sep=''))
		}else{
		   title(titulo)
		}
		if(color){
		  if(is.na(vd[1])) {
		    vd='vd'
		  }else{
		    nombres=names(dat_)
		    col.vd=match(vd,nombres)
		    nombres[col.vd]='vd'
		    names(dat_)=nombres
		  }
		  legend(posicion[1],posicion[2],legenda, cex=0.8, col=1:dim(tabla)[2],pch=19);
		}else{
		  legend(posicion[1],posicion[2],legenda, cex=0.8, lty=1:dim(tabla)[2]);
		}				
		if(!is.na(verticales[1])) abline(v=verticales, lty=2)
	} # FIN INTER. DOBLES
	
	# INT. TRIPLES
	if(n.fac==3){
		fac1=facts[[1]][1]; fac2=facts[[1]][2]; fac3=facts[[1]][3]
		if(is.na(pivote[1])) pivote=1
		join.fac=c(fac1,fac2,fac3)

		# Declaramos pivote
		pivot=join.fac[pivote];	join.fac=join.fac[-pivote]
		  x.pivote=split(dat_, dat_[,pivot])
		  tabla=lapply(x.pivote,function(x){
			with(x,tapply(vd,list(eval(parse(text=join.fac[1])),
			eval(parse(text=join.fac[2]))),function(x) mean(x,na.rm=TRUE)))
			})

		  etiquetas=rownames(tabla[[1]])
 		  legenda=levels(dat_[,join.fac[2]])

		  if(is.null(legenda)){
		    legenda=unique(dat_[,fac2])
		    cat('', fill=TRUE)
		    cat('*** WARNING. La variable incluida como segundo termino de la interaccion no es factor',fill=TRUE)
		    cat('*** se han seleccionado los valores no repetidos de esa variable como niveles para ese',fill=TRUE)
		    cat('*** factor. Si deseas un ordenamiento distinto al alfabetico utiliza la funcion:',fill=TRUE)
		    cat('*** reorder.factor.fnc.',fill=TRUE)
		    cat("*** Ej: datos = reordera.factor.fnc(dat, que.factor='grupo', niveles=c('C','B','A'))",fill=TRUE)
		  } 
		  
		  minimo=round(min(do.call(rbind,tabla)),1);
		  minimo=minimo-0.1*minimo
		  maximo=round(max(do.call(rbind,tabla)),1);
		  maximo=maximo+0.1*maximo
		  if (is.na(ylim)[1]) ylim=c(minimo,maximo)
		  colores=length(unique(as.character(dat_[,join.fac[2]])))

		# GRAFICA
		for(j in 1:length(tabla)){
		  x=tabla[[j]]
		  X11( )
		  plot(x[,1],type='l',lwd=2,axes=FALSE, ann=FALSE, ylim=ylim)
 		   	axis(1, at=1:length(etiquetas), lab=etiquetas, cex.axis=size.font)
			axis(2)
			if(color){
			  for(i in 2:dim(x)[2])lines(x[,i],type='l',lwd=2,col=i)
			}else{
			  for(i in 2:dim(x)[2])lines(x[,i],type='l',lwd=2,lty=i)
			}
		
 			box()
 			title(xlab=join.fac[1])
 			title(ylab=ylab)
			if(is.na(posicion[1])){
				n.label=length(etiquetas)
				pos1=floor((n.label*80)/100)
				pos2=ceiling((ylim[2]*80)/100)
				posicion=c(pos1,pos2)
			}
			if(is.na(titulo[1])){
 			   title(main=paste('Interaccion: ',join.fac[1],':',join.fac[2],
				' en ',names(tabla)[j],sep=''))
			}else{
			   title(titulo)
			}
			if(color){
			  legend(posicion[1],posicion[2],legenda, cex=0.8, col=1:dim(x)[2],pch=19);
			}else{
			  legend(posicion[1],posicion[2],legenda, cex=0.8, lty=1:dim(x)[2]);
			}
			
			if(!is.na(verticales[1])) abline(v=verticales, lty=2)
		} # FIN GRAFICA
	} # FIN INT. TRIPLES
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# FUNCION:
#      compara.my.factors.fnc(datos,factor.a.comparar,repetidas=TRUE,vd=NA)
# Lleva a cabo las J(J-1)/2 comparaciones par a par de un factor, la
# data de entrada puede ser una lista (para efectos simples) como un
# data.frame (pos-hoc en efectos principales.
#---------------------------------------------------------------------------
 compara.pares.fnc=function(datos,factor.a.comparar,repetidas=TRUE,vd=NA){
	if (is.na(vd)) vd='vd'
	x.factor=datos; fac.comp=factor.a.comparar
	nlev.A=nlevels(x.factor[[1]][,fac.comp])
	Xi=matrix(c(1:nlev.A),1,nlev.A); Xi
	indi=tot.pares.fnc(Xi)
	if (is.matrix(indi)) hasta=dim(indi)[1] else  hasta=1
	que.labels=etiqueta.fnc(levels(x.factor[[1]][,fac.comp]),indi)
	if(is.list(x.factor)) {
		seg.agregado=lapply(x.factor, function(x){
					seg.agrega=with(x,aggregate(x[,'vd'],
						list(sujeto,eval(parse(text=fac.comp))),function(x) mean(x,na.rm=TRUE)))
					colnames(seg.agrega)=c('sujeto',fac.comp,vd)
					return(seg.agrega)})
		resultado=lapply(seg.agregado, function(x) {
			storage=matrix(0,hasta,3)
			for (i in 1:hasta){
    			etiq=strsplit(que.labels[i],'-');
				etiq.1=etiq[[1]][1]; etiq.2=etiq[[1]][2]
				que.data=subset(x,
					x[,fac.comp]==etiq.1 | x[,fac.comp]==etiq.2)
				if(repetidas) {
					que.data[,fac.comp]=que.data[,fac.comp]
					tabla=with(que.data,tapply(vd,
						list(sujeto,eval(parse(text=fac.comp))),
							function(x) mean(x,na.rm=TRUE)))
					chivato=apply(is.na(tabla),2,sum)
					indice=chivato==dim(tabla)[1]
					tabla=tabla[,!indice]
					tabla=data.frame(na.omit(tabla))
					contraste= try(t.test(tabla[,1],tabla[,2],paired=TRUE),silent=TRUE)
				} else {
					contraste=try(t.test(que.data[,'vd'] ~ que.data[,fac.comp],
						paired =FALSE, var.equal =FALSE ),silent=TRUE)
				}
				if(class(contraste)=='try-error') {
				  t=NA; df=NA; p=NA
				}else{
				  t=contraste$statistic; df=contraste$parameter
				  p=round(contraste$p.value,5)
				}
				storage[i,]=cbind(t=t, df=df, p=p)
			}
			storage=as.data.frame(storage)
			row.names(storage)=que.labels
			colnames(storage)=c('t','df','p')
			return(storage)})
 		return(resultado)
	} else {
			storage=matrix(0,hasta,3)
			for (i in 1:hasta){
    				etiq=strsplit(que.labels[i],'-');
				etiq.1=etiq[[1]][1]; etiq.2=etiq[[1]][2]
				que.data=subset(x,
					x[,fac.comp]==etiq.1 | x[,fac.comp]==etiq.2)
				if(repetidas) {
					contraste=try(t.test(que.data[,vd] ~ que.data[,fac.comp],
						paired = T,var.equal =FALSE ),silent=TRUE)
				} else {
					contraste=try(t.test(que.data[,vd] ~ que.data[,fac.comp],
						paired =FALSE, var.equal =FALSE ),silent=TRUE)
				}
				if(class(contraste)=='try-error') {
				  t=NA; df=NA; p=NA
				}else{
				t=contraste$statistic; df=contraste$parameter
				p=round(contraste$p.value,5)
				}
				storage[i,]=cbind(t=t, df=df, p=p)
			}
			storage=as.data.frame(storage)
			row.names(storage)=que.labels
			colnames(storage)=c('t','df','p')
 	return(storage)
	}
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# clean.VD.fnc(dat,'vd',Z=FALSE,que.corte=2.5)
#---------------------------------------------------------------------------
 limpia.vd.fnc=function(datos,vd,que.factor,Z=TRUE,que.corte=2,que.minimo, que.maximo=NA){
	# Check si existe la variable RT y la sustituye
	RT_=FALSE
	if(vd=='RT'){
	  RT_=TRUE
	  datos=cambia.nombre.var.fnc(datos,'RT','RT_2',silente=TRUE)
	  vd='RT_2'
	}
	
	RT=vd
	dat=datos
	crea.cat.fnc('LIMPIA LA VARIABLE DEPENDIENTE POR SUJETO Y CONDICION')
	check.que.factor(que.factor)

 # Pone un orden por record
	dat$index=1:dim(dat)[1]
	n.dim=dim(dat)[1]
	my.row.names=rownames(dat)
 if(Z) method=paste('Z+- ',que.corte,'sd',sep='') else
	  method=paste('Q(1,3)+- ',que.corte,'mad',sep='')

 # Divide la data en rt positivos y negativos si existen los hace NA
	dat.NA=dat[is.na(dat[,RT]),]
	dim.dat.NA=dim(dat.NA)[1]
	if(dim.dat.NA > 0) dat=dat[!is.na(dat[,RT]),]
	n.dim=dim(dat)[1]
	if(is.na(que.maximo[1])){
	  dat.yes=subset(dat, dat[,RT] >= que.minimo)
	}else{
	  dat.yes=subset(dat, dat[,RT] >= que.minimo & dat[,RT] <= que.maximo)
	}
	if(dim(dat.yes)[1]==0){
		cat('',fill=TRUE)
		cat('*** Error. El filtraje impuesto en los argumentos que.minimo o',fill=TRUE)
		cat('*** que.maximo parece ser incorrecto dado que se eliminan todas',fill=TRUE)
		cat('*** las observaciones de la base de datos. Compruebalo.',fill=TRUE)
		cat('',fill=TRUE)
		stop( )
	}
	if(dim(dat.yes)[1] < n.dim){
		if(is.na(que.maximo[1])){
		  dat.no=subset(dat, dat[,RT] < que.minimo)
		}else{
		  dat.no=subset(dat, dat[,RT] < que.minimo | dat[,RT] > que.maximo)
		}
		dat.no[,RT]=NA
	}
	
	tabla.antes=frecuencias.fnc(dat.yes, paste('sujeto:',que.factor,sep=''),silente=TRUE)[[2]]
	by.subject=split(dat.yes,dat.yes$sujeto)
	by.subject=lapply(by.subject, function(x) {
		x[,que.factor]=as.character(x[,que.factor])
		by.factor=split(x,x[,que.factor])
 #COMIENZA BUBLE POR CONDICION
		by.factor=lapply(by.factor, function(x) {
 # Si solo hay un record lo devuelve
			if(dim(x)[1] == 1) return(x)
			if(length(unique(x[,RT]))==1) return(x)
 # Si limpiamos por MAD
			if(!Z) {
				Q=quantile(x[,RT],na.rm=TRUE)
	  			irq=mad(x[,RT],na.rm=TRUE)
	  			crit.sup=Q[3]+que.corte*irq
	  			crit.inf=Q[1]-que.corte*irq
	  			yes=subset(x,x[,RT] > crit.inf & x[,RT] < crit.sup)
	  			if(dim(yes)[1] == dim(x)[1]) {
					joined=yes
	  			}else{
	  				no=subset(x,x[,RT] <= crit.inf | x[,RT] >= crit.sup)
					no[,RT]=NA
	 				joined=rbind(yes,no)
	 				joined=joined[order(joined$index),]
	  			}
			}else{
 # Limpia por Z
				yes=subset(x,abs(tipifica.fnc(x,RT)) <= que.corte)
				if(dim(yes)[1] == dim(x)[1]) {
					joined=yes
	 			}else{
	 				no=subset(x,abs(tipifica.fnc(x,RT)) > que.corte)
	 				no[,RT]=NA
	 				joined=rbind(yes,no)
	 				joined=joined[order(joined$index),]
				}
			}
		return(joined)
		}) # CIERRE DEL LOOP BY FACTORS
		return(do.call(rbind,by.factor))}) # CIERRE LOOP BY SUBJECT

 	by.subject=do.call(rbind,by.subject)
 	by.subject=reordena.factor.fnc(by.subject, que.factor=que.factor, silente=T,
		niveles=levels(as.factor(dat.yes[,que.factor])))
 	tabla.despues=frecuencias.fnc(by.subject[!is.na(by.subject[,vd]),], paste('sujeto:',que.factor,sep=''),silente=TRUE)[[2]]
	dif=tabla.antes-tabla.despues
	dif=as.data.frame(dif)
	dif=cambia.nombre.var.fnc(dif, 'Freq','vd',silente=TRUE)
	dif=unstack(dif, form=paste('vd ~ ',que.factor,sep=''))
	dif$Total.NA=apply(dif,1,sum)
	row.names(dif)=dimnames(as.table(tabla.antes))[[1]]
	
 # INTEGRA LAS PARTES MENOR Y MAYOR QUE CERO
	if(dim(dat.yes)[1] < n.dim){
		join2=rbind(by.subject,dat.no)
	} else {
		join2=by.subject
	}
# NA DESCRIPTIVES
 new.NA=join2[is.na(join2[,RT]),]

 if(dim(new.NA)[1] > 0){
 	table.new.NA=frecuencias.fnc(new.NA, que.factor, silente=TRUE)[[1]]$tabla
 	no.NA=dim(join2[!is.na(join2[,RT]),])[1]
	si.NA=dim(new.NA)[1]
	total=no.NA+si.NA
	prop.new.NA=(si.NA/total)*100
 }else{
	table.new.NA=0
 }
 if(dim.dat.NA > 0) {
 	table.previous.NA=frecuencias.fnc(dat.NA, que.factor, silente=TRUE)[[1]]$tabla
 	}else{
	table.previous.NA=0
 }
# Integra NA
	if(dim.dat.NA > 0) join2=rbind(join2,dat.NA)
	join2=join2[order(join2$index),]
	rownames(join2)=1:dim(join2)[1]
	n.col.index=dim(join2)[2]
	join2=join2[,-n.col.index]
	if(RT_) {
	  join2=cambia.nombre.var.fnc(join2, 'RT_2','RT', silente=TRUE)
	  vd='RT'
	}

 lista=list(metodo=method,previous.NA=table.previous.NA, freq.new.NA=table.new.NA,
	porcentaje.new.NA=paste(round(prop.new.NA,3),'%',sep=''),
	n.items.eliminados.x.sujeto.condicion=dif)
 print(lista)
 print(histograma.fnc(join2, vd=vd, que.factor=que.factor, check=TRUE))
 return(join2)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# agrega.los.datos.fnc(datos.apilados,c('sujeto','factorA','factorB'),'media')
#---------------------------------------------------------------------------
 agrega.los.datos.fnc=function(datos=NA, que.factor,estadistico='media',
							silente=FALSE,vd=NA){
	if(class(try(is.na(datos)))=='logical') {
		cat('',fill=TRUE)
		crea.cat.fnc('agrega.los.datos.fnc')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(" # Creamos unos datos ficticios de 100 sujetos en 20 items  		",fill=TRUE)
		cat("  datos=data.frame(mvrnorm(20,rep(0,20),Sigma=diag(1,20)))			",fill=TRUE)
		cat(" # Asignamos item como nombre de cada columna					",fill=TRUE)
		cat("  datos=crea.nombre.item.fnc(datos)           					",fill=TRUE)
		cat(" # Declaramos la estructura intra del design					",fill=TRUE)
		cat("  fac.intra=list(A=c('a1','a2'), B=c('b1','b2'))				",fill=TRUE)
		cat(" # Apilamos los datos										",fill=TRUE)
		cat("  datos.ap=apila.los.datos.fnc(datos, fac.intra=fac.intra,			",fill=TRUE)
		cat("         col.empieza.item=1, n.item=20)						",fill=TRUE)
		cat(" # Agregamos por sujetos y por items 							",fill=TRUE)
		cat("  agrega.sujeto=agrega.los.datos.fnc(datos.ap,					",fill=TRUE)
		cat("         que.factor=c('sujeto','A','B'))					",fill=TRUE)
		cat(" # Agregamos por sujetos y por items 							",fill=TRUE)
		cat("  agrega.item=agrega.los.datos.fnc(datos.ap,					",fill=TRUE)
		cat("         que.factor=c('item','A','B'))						",fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Genera estadisticos de resumen de una variable cuantitativa en el   ', fill=TRUE)
		cat(' cruce de J factores definidos por el usuario.					', fill=TRUE)
		cat(' Si no se utiliza el argumento estadistico, se utilizara la media. 	', fill=TRUE)
		cat(' Los estadisticos de resumen que pueden solicitarse son ademas de 	', fill=TRUE)
		cat(' la media, la desviacion tipica (dt), el numero de observaciones (n)	', fill=TRUE)
		cat(' y la suma (suma).										', fill=TRUE)
		cat(" ",fill=TRUE)
		cat("sites.google.com/site/ullrtoolbox/lectura-de-archivos/agrega-los-datos-fnc", fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		print(argumentos('agrega.los.datos.fnc'))
	return('help')
	}
	
	hay.dosp=grep(':',que.factor)
	if(length(hay.dosp)==1){
	  que.factor= strsplit(que.factor,':')[[1]]
	}else{
	  que.factor= que.factor
	}
	
	dat.st=datos
	nombres=names(dat.st)
	if(is.na(vd[1])){
		vd.na=TRUE; vd='vd'
	}else{
		vd.na=FALSE
	}
	if(is.numeric(vd[1])) vd=nombres[vd]
	if(is.na(match(vd[1],names(datos)))){
		if(vd.na){
			cat('*** Error. No has declarado el argumento vd. Este es obligatorio',fill=TRUE)
			cat('*** si no existe una variable con el nombre vd en la base',fill=TRUE)
			cat("*** de datos que has introducido. Ej: vd='tiempo'        ",fill=TRUE)
			cat('',fill=TRUE)
		stop( )
		}
		cat('',fill=TRUE)
		cat('*** Error. No existe la variable',vd,'en tu base de datos ***',fill=TRUE)
		cat('',fill=TRUE)
		stop( )
	}

	x.sujeto=match('sujeto',que.factor)
	x.item=match('item',que.factor)

	if(is.na(x.sujeto)) x.sujeto=0
	if(is.na(x.item)) x.item=0

      fac=que.factor
      n.fac=length(que.factor)
      
      indice=fac %in% nombres
      if(sum(indice)!= n.fac){
		cat('', fill=TRUE)
		cat('*** Error, el o los siguientes factores incluidos en el argumento que.factor ***',fill=TRUE)
		cat('*** no existen en la base de datos.'                                      ,fill=TRUE)
		print(fac[!indice])
		stop()
		cat('', fill=TRUE)
      }    
      if(is.numeric(vd)) vd=nombres[vd]
      indice=vd %in% nombres
      if(sum(indice)!= length(vd)){
		cat('', fill=TRUE)
		cat('*** Error, la o las siguientes variables incluidas en el argumento vd ***',fill=TRUE)
		cat('*** no existen en la base de datos.'                                      ,fill=TRUE)
		print(vd[!indice])
		stop()
		cat('', fill=TRUE)
      }
      dat.st=dat.st[,c(vd,que.factor)]
      lf=que.factor
      acumula=lf[1]
      for(i in 2:length(que.factor)) acumula=paste(acumula,'+',lf[i], collapse ='+') 
      modelo=paste('.~',acumula,sep='')
    
      if(estadistico=='media')
		agregado=aggregate(eval(parse(text=modelo)), data=dat.st, function(x) mean(x, na.rm=TRUE))
      if(estadistico=='dt')
		agregado=aggregate(eval(parse(text=modelo)), data=dat.st, function(x) sd(x, na.rm=TRUE))   
      if(estadistico=='suma')
		agregado=aggregate(eval(parse(text=modelo)), data=dat.st, function(x) sum(x, na.rm=TRUE))   
      if(estadistico=='n')
		agregado=aggregate(eval(parse(text=modelo)), data=dat.st, length)   
      if(x.sujeto==1) agregado=agregado[order(agregado$sujeto),]
      if(x.item==1) agregado=agregado[order(agregado$item),]
      if(!silente){
		cat('',fill=TRUE)
		cat('*** Esta es la cabecera de tus datos agregados con el estadistico',estadistico,'***',fill=TRUE)
		cat('',fill=TRUE)
		print(head(agregado))
      }
  return(agregado) 
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# grafica.dispersion.fnc(dat,variables,que.factor='gr.edad')
#---------------------------------------------------------------------------
 grafica.dispersion.fnc=function(datos,variables=NA, que.factor=NA){
        require(car, quietly = TRUE)
        dat=datos
	   check.que.factor(que.factor)

        if(is.na(variables[1])) variables=names(dat)
        if(is.numeric(variables)){
                nombres=names(dat)
                que.var=nombres[variables]
                variables=que.var
        }

	vari=variables
	if(length(variables) < 2){
		cat('',fill=TRUE)
		cat('*** Error. Debes incluir al menos dos variables cuantitativas ***',fill=TRUE)
		cat('',fill=TRUE)
		stop( )
	}
	acumula=variables[1]

	if(is.na(que.factor[1])) dat=na.omit(dat[,variables]) else 
	    dat=na.omit(dat[,c(variables,que.factor)])
	if(dim(dat)[1]==0){
	  cat('',fill=TRUE)
	  cat('*** Error. Despues de eliminar los datos perdidos no quedan casos en la matriz ***',fill=TRUE)
	  cat('',fill=TRUE)
	  stop( )
	}	

	# ACUMULADOR
	for(i in 2:length(variables))
		acumula=paste(acumula,variables[i],sep='+',collapse='+')
	if(is.na(que.factor[1])){
		modelo=paste('~',acumula)
	}else{
		if(length(que.factor)!=1){
			cat('',fill=TRUE)
			cat('*** Error. Solo puedes definir un factor en el argumento que.factor ***',fill=TRUE)
			cat('',fill=TRUE)
			stop( )
		}
		chivato=strsplit(que.factor,':')[[1]]
		if(length(chivato)!=1){
			cat('',fill=TRUE)
			cat('*** Error. Solo puedes definir un factor en el argumento que.factor ***',fill=TRUE)
			cat('',fill=TRUE)
			stop( )
		}
		modelo=paste('~',acumula,'|',que.factor)
	}
     scatterplotMatrix(eval(parse(text=modelo)), data=dat)
	#try(detach(package:car),silent=TRUE)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# fiabilidad.fnc(dat,tipo='omega',claves=c(1,1,-1,-1,1,1))
#---------------------------------------------------------------------------
# escalas=list(	Acuerdo=c(-1,2:5),
#			Consciente=c(6:8,-9,-10),
#			Extraversion=c(-11,-12,13:15),
#			Neuroticismo=c(16:20),
#			Apertura = c(21,-22,23,24,-25)
#			)
# escalas
#fiabilidad.fnc(bfi,variables=1:25,escalas=escalas)

 fiabilidad.fnc=function(datos,variables=NA, tipo=NA, escalas=NA,claves=NA, latex=FALSE, 
														silente=FALSE, n.factores=NA){
	require(psych, quietly = TRUE)
	if(is.na(tipo[1])) tipo='alfa'
	dat=datos
	if(is.na(variables[1])){
		dat=datos
	}else{
		dat=dat[,variables]
	}

	crea.cat.fnc('ANALISIS DE ESCALAS')
	if(is.na(escalas[1])){
    		if(tipo=='alfa' & is.na(claves)[1]) fiabi=alpha(dat)
    		if(tipo=='alfa' & !is.na(claves)[1]) fiabi=alpha(dat,keys=claves)
    		if(tipo =='guttman' & is.na(claves)[1]) fiabi=guttman(dat)
    		if(tipo=='guttman' & !is.na(claves)[1]) fiabi=guttman(dat,keys=claves)
    		
    		if(tipo=='omega' & is.na(claves)[1]){
				if(is.na(n.factores[1]))
					n.factores=fa.parallel_(dat,fm="minres", fa="fa", n.iter=20,
						error.bars=FALSE,SMC=FALSE)		
				fiabi=omega(dat, nfactors=n.factores)
			}	
    		if(tipo=='omega' & !is.na(claves)[1]){
				if(is.na(n.factores[1]))
					n.factores=fa.parallel_(dat,fm="minres", fa="fa", n.iter=20,
						error.bars=FALSE,SMC=FALSE)
				fiabi=omega(dat,keys=claves,nfactors=n.factores)
			}	
	}else{
		chivato=is.list(escalas)
		if(!chivato) {
			cat('',fill=TRUE)
			cat('*** Error, el argumento escalas debe ser una lista que contenga    ***',fill=TRUE)
			cat('*** los nombres que quieres dar a las escalas con los numeros de   ***',fill=TRUE)
			cat('*** columnas de los items que las forman.                          ***',fill=TRUE)
			cat('',fill=TRUE)
			cat('Ej. escalas=list(ansiedad=c(1,-2,3:5), extra=c(6,-7,10,12))        ***',fill=TRUE)
			cat('El signo negativo de la columna indica que el item esta invertido. ***',fill=TRUE)
			stop( )
		}
		vector=integer( )
 		for(i in escalas)	vector=c(vector,abs(i))
		vector=sort(vector)
		vector_=1:length(vector)
		vector_=data.frame(cbind(vector,vector_))
		escalas_=escalas
		escalas.p=escalas

		# REESCALA LA ESCALA
		escalas=lapply(escalas, function(x){
			for(i in 1:length(x))
				x[i]=vector_[vector_$vector==abs(x[i]),'vector_']
			return(x)
		})
		
		escalas_=lapply(escalas_, function(x) {
			f1=as.numeric(x < 0)-2
			indice=f1==-2
			f1[indice]=1
			return(f1)
			})
			
		for(j in 1:length(escalas))
			escalas[[j]]=escalas[[j]]*escalas_[[j]]
		
		# FIN REESCALA ESCALA
		dat=datos[,vector]
		x.nombres=lapply(escalas, function(x) names(dat)[abs(x)])
		#for(i in 1:dim(dat)[2]) dat[,i]=as.integer(dat[,i])
 		n.var=do.call(sum,lapply(escalas, function(x) length(x)))
 		key= make.keys(nvars=n.var, escalas, item.labels=names(dat) )
 		fiabi= score.items(key,dat)
		fiabi=list(escalas=escalas.p,escalas=x.nombres,numero.items=fiabi$n.items,
 			alfa.escala=fiabi$alpha,
 			promedio.cor.item.intra.escala=fiabi$av.r,
 			cor.item.escala=fiabi$item.cor,
 			cor.item.escala.atenuada=fiabi$item.corrected,
 			cor.entre.escalas=fiabi$cor,
 			cor.alfa.cor.atenuada.alfa.en.diagonal=fiabi$corrected,
 			gutman=fiabi$G6,
 			frecuencias=fiabi$response.freq)
	}
 try(detach(package:psych),silent=TRUE)
 if(silente) return(fiabi) else print(fiabi)
 if(latex & !silente) latex.fnc(fiabi)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# reordena.factor.fnc(datos,factor,niveles)
# reordena.factor.fnc(dat,'tratamiento',niveles=c('control','A'))
# Define los nuevos niveles para un factor.
#---------------------------------------------------------------------------
 reordena.factor.fnc=function(datos=NA,que.factor,niveles,hacer.NA=FALSE,silente=NA){

	if(class(try(is.na(datos)))=='logical') {
		cat('',fill=TRUE)
		crea.cat.fnc('reordena.factor.fnc')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat("  datos = reordena.factor.fnc(OBrienKaiser, que.factor='treatment',",fill=TRUE)
		cat("  		niveles=c('control','A','B') )                          ",fill=TRUE)
		cat("  datos = reordena.factor.fnc(datos, que.factor='tr.r',            ",fill=TRUE)
		cat("  		niveles=c('Q1','Q4'), hacer.NA=T)                       ",fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Permite cambiar el orden de los niveles de un factor desde su ordenamiento .', fill=TRUE)
		cat(' por defecto de caracter alfabetico a un nuevo orden definido por el usuario.', fill=TRUE)
		cat(' Otra de las opciones de gran utilidad de esta funcion esta en eliminar ', fill=TRUE)
		cat(' (haciendo NA) aquellos niveles de un factor que el usuario desee.      ', fill=TRUE)
		cat(' Asimismo permite cambiar la clase de pertenencia de una variable       ', fill=TRUE)
		cat(' (numerica, entera o caracter) a la clase factor.                       ', fill=TRUE)
		cat(" ",fill=TRUE)
		cat(" sites.google.com/site/ullrtoolbox/02-manipulacion-de-datos/reordena-factor-fnc", fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		print(argumentos('reordena.factor.fnc'))
	return('help')
	}

    if(is.na(silente[1])) crea.cat.fnc('REORDENANDO NIVELES DEL FACTOR')
	dat=datos
	check.que.factor(que.factor)

	if(!is.factor(datos[,que.factor]))
		dat=transforma.variable.fnc(dat, variable=que.factor, nuevo.tipo='factor',silente=TRUE)

	old.lev=levels(dat[,que.factor])
	if( length(niveles)!= length(old.lev) & !hacer.NA){
		cat('*** Error, el numero de niveles introducidos no coinciden con',fill=TRUE)
		cat('*** el numero de niveles actual de la variable.',fill=TRUE)
		cat('',fill=TRUE)
		cat(' Estos son los niveles reales de la variable',fill=TRUE)
		print(old.lev)
		cat('',fill=TRUE)
		cat(' y estos son los que pretendes reordenar',fill=TRUE)
		print(niveles)
		cat('',fill=TRUE)
		cat('*** Si lo que deseas es ordenar solo algunos niveles y hacer NA',fill=TRUE)
		cat('*** los omitidos, introduce el argumento hacer.NA=T',fill=TRUE)
	 return(datos)
	}
	encaje=match(old.lev,niveles)
	check=sum(is.na(encaje))
	if(check != 0 & !hacer.NA){
		cat('*** Error. Algunos de los niveles a reordenar no coincide con',fill=TRUE)
		cat('*** los niveles reales de la variable',fill=TRUE)
		cat('',fill=TRUE)
		cat('*** Estos son los niveles reales de la variable:',fill=TRUE)
		print(old.lev)
		cat('',fill=TRUE)
		cat('*** y estos son los que pretendes reordenar',fill=TRUE)
		print(niveles)
		cat('',fill=TRUE)
		cat('*** Si lo que deseas es ordenar solo algunos niveles y hacer NA',fill=TRUE)
		cat('*** los omitidos, introduce el argumento hacer.NA=T',fill=TRUE)
		cat('*** Si por el contrario lo que deseas es reordenar todos los niveles',fill=TRUE)
		cat('*** modifica los erroneos introducidos en el argumento niveles',fill=TRUE)
	return(datos)
	}
 	dat[,que.factor]=as.character(dat[,que.factor])
 	dat[,que.factor]=factor(dat[,que.factor],levels=niveles)
 	new.lev=levels(dat[,que.factor])
# 	if(hacer.NA) {
# 		dat=dat[!is.na(dat[,que.factor]),]
#    	cat('*** Se han eliminado los casos con niveles omitidos en el factor',que.factor,' ***',fill=TRUE)
#    	cat('-----------------------------------------------------------------------------',fill=TRUE)#
#	}
 	lista=list(hacer.NA=hacer.NA,niveles.antiguos=old.lev, niveles.nuevos=new.lev)
    	if(is.na(silente[1])){
		print(lista)
		cat('*** Tabla de frecuencias con el nuevo orden:',fill=TRUE)
		print(frecuencias.fnc(dat,que.factor,silente=TRUE)[[1]]$tabla)
	}
 return(dat)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# discretiza.variable.fnc(dat,variable='age',ntiles=4,cortes=NA,etiquetas=NA
#---------------------------------------------------------------------------
 discretiza.variable.fnc=function(datos=NA,variable,var.out=NA,ntiles=NA, ordinal=NA,
		cortes=NA, descendente=TRUE, etiquetas=NA, categorias=NA,
		que.factor=NA, silente=NA){

	if(class(try(is.na(datos)))=='logical') {
		cat('',fill=TRUE)
		crea.cat.fnc('discretiza.variable.fnc')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat("  datos=discretiza.variable.fnc(datos, variable='tr', ntiles=4)      ",fill=TRUE)
		cat("  datos=discretiza.variable.fnc(datos, variable='tr', ntiles=4,      ",fill=TRUE)
		cat("  		que.factor='curso')                                       ",fill=TRUE)
		cat("  datos=discretiza.variable.fnc(datos, variable='tr', ntiles=10)     ",fill=TRUE)
		cat("  datos=discretiza.variable.fnc(datos, variable='tr', ordinal=T)     ",fill=TRUE)
		cat("  datos=discretiza.variable.fnc(datos, variable='tr', cortes=5)      ",fill=TRUE)
		cat("  datos=discretiza.variable.fnc(datos, variable='tr',                ",fill=TRUE)
		cat("  		cortes=c(10,20,30,60))                                    ",fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Genera una nueva variable a partir de la discretizacion de otra definida.', fill=TRUE)
		cat(' El usuario puede elegir un criterio percentilico (ntiles), ordinal o de', fill=TRUE)
		cat(' cortes arbitrarios predefinidos.                                       ', fill=TRUE)
		cat(" ",fill=TRUE)
		cat(" sites.google.com/site/ullrtoolbox/02-manipulacion-de-datos/discretiza-variable-fnc ", fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		print(argumentos('discretiza.variable.fnc'))
	return('help')
	}

	dat=datos
	check.que.factor(que.factor)

	if(is.numeric(variable)){
		nombres=names(dat)
		variable=nombres[variable]
	}

    vari=variable
	# CHECK SI EL FACTOR EXISTE
	if(!is.na(que.factor[1])) {
		chivato=existe.variable.fnc(datos,que.factor)
		if(chivato$cc !=0){
			cat('',fill=TRUE)
			cat('*** Error. No existe el factor incluido en el argumento que.factor ***',fill=TRUE)
			cat('*** Tu factor:',chivato$faltan,fill=TRUE)
			cat('',fill=TRUE)
			stop( )
		}
	}
	# FIN CHECK

	# SI NTILES POR NIVEL DE UN FACTOR.
	if(!is.na(que.factor[1]) & !is.na(ntiles[1])){
		dat=crea.indice.percentilico.x.factor.fnc(dat, variable, ntiles,
			que.factor,var.out)

		return(dat)
	} # FIN DE NTILES POR NIVEL DE FACTOR

	# SI ORDINAL
	if(!is.na(ordinal[1])){
		que.clase=class(dat[,variable])
		if(que.clase == 'integer' | que.clase=='numeric'){
			dat=crea.indice.ordinal.fnc(dat,variable,que.factor,var.out,descendente)
			return(dat)
		}else{
			cat('',fill=TRUE)
			cat('*** Error. El argumento ordinal=T solo es aplicable a variables de tipo numerico ***',fill=TRUE)
			cat('*** Revisa y modifica la variable incluida en el argumento variable.             ***',fill=TRUE)
			cat('*** variable:',vari,fill=TRUE)
			cat('',fill=TRUE)
			stop( )
		}
	} # FIN SI ORDINAL

   if(is.na(silente[1])){
      crea.cat.fnc('DISCRETIZADO DE VARIABLE CONTINUA')
      cat(paste('$tronco.y.hoja.',variable,sep=''),fill=TRUE)
    }

    if(!is.na(ntiles[1])) {
		metodo='Cortes Percentilicos'
		if(ntiles > 4 & ntiles < 9){
		  cat('',fill=TRUE)
 		  cat('*** Error. El argumento ntiles solo puede valer 2,3,4 o 10 para los deciles ***',fill=TRUE)
		  stop( )
		}
    }

    if(!is.na(cortes[1]) & length(cortes)==1) { categorias=cortes; cortes=NA }

    # SI HAY CORTES DEFINIDOS POR EL USUARIO
    if(!is.na(cortes[1])){
      metodo='Cortes definidos por el usuario'
      if(!is.na(etiquetas[1])){
		di=length(cortes)-length(etiquetas)
		if(di != 1) {
	  		cat(' ',fill=TRUE)
	  		cat('***                           ERROR                                 ***',fill=TRUE)
	  		cat('*** El numero de cortes ha de ser 1 valor mayor que el de etiquetas ***',fill=TRUE)
	  	return(dat)
		}
      }
    } # FIN DE CORTES DEFINIDOS POR USUARIO

    if(!is.na(categorias[1])){
        metodo=paste('Cortes equiespaciados. Categorias: ',categorias,sep='')
        if(is.na(var.out)) new.fac=paste(variable,'.c',sep='') else new.fac=var.out
	if(is.na(que.factor[1])){
        	dat[,new.fac]=cut(dat[,vari], categorias, labels=FALSE, na.rm=TRUE)
                if(is.na(silente[1])){
		  print(descriptivos.fnc(dat, vd=vari, que.factor=new.fac))	
                  cat('-----------------------------------------------------------------------------',fill=TRUE)
                  cat('***  Se ha creado la variable',new.fac,'discretizado por categorias de la variable',variable,'***',fill=TRUE)
                  cat('-----------------------------------------------------------------------------',fill=TRUE)
                }
	}else{
		size=dim(dat)
		dat$ID=1:dim(dat)[1]; que.col.ID=size[2]+1
		x.factor=divide.por.factor.fnc(dat, que.factor=que.factor, silente=TRUE)
		x.factor=lapply(x.factor, function(x) {
		   x[,new.fac]=cut(x[,vari], categorias, labels=FALSE, na.rm=TRUE)
		   return(x)})
		dat_=do.call(rbind,x.factor)
		dat_=dat_[order(dat$ID),]
		dat=dat_[,-c(que.col.ID)]
                if(is.na(silente[1])){
		  print(descriptivos.fnc(dat, vd=vari, que.factor=paste(new.fac,':',que.factor,sep='')))	
                  cat('-----------------------------------------------------------------------------',fill=TRUE)
                  cat('***  Se ha creado la variable',new.fac,'discretizado por categorias de la variable',variable,'***',fill=TRUE)
                  cat('***  en cada nivel del factor: ', que.factor,                                '***',fill=TRUE)
                  cat('-----------------------------------------------------------------------------',fill=TRUE)
                }
	}
    return(dat)
    } # FIN DE CORTES CATEGORIAS EQUIESPACIADAS

    if(!is.na(ntiles) & !is.na(cortes[1]))
      cat('*** Has definido cortes por percentiles y manuales elige uno de los dos ***',fill=TRUE)

  	# SIN CORTES Y ETIQUETAS
	if(is.na(cortes[1]) & is.na(etiquetas[1]) & is.na(categorias[1])) {
     		lab2=c('< P50','> P50')
     		lab3=c('< P33','P33-P66','> P66')
     		lab4=c('Q1','Q2','Q3','Q4')
     		if(is.na(var.out)) new.fac=paste(variable,'.r',sep='') else new.fac=var.out
     		if(ntiles==2) dat[,new.fac]=cut(dat[,vari],labels=lab2, quantile(dat[,vari],
			c(0,0.5,1),na.rm=TRUE,include.lowest = T))
		if(ntiles==3) dat[,new.fac]=cut(dat[,vari],labels=lab3, quantile(dat[,vari],
			c(0,0.33,0.66,1),na.rm=TRUE),include.lowest = T)
     		if(ntiles==4){
			Q=quantile(dat[,vari],seq(0,1,0.25),na.rm = T)
			dif=length(Q)-length(unique(Q))
			if(dif == 1 & Q[1]==Q[2]) Q[1]=Q[1]-1
			if(dif == 1 & Q[2]==Q[3]) Q[2]=NA
			if(dif == 1 & Q[3]==Q[4]) Q[3]=NA
			if(dif == 1 & Q[4]==Q[5]) Q[5]=Q[5]+1
			dat[,new.fac]=cut(dat[,vari],lab=lab4, Q ,include.lowest = T)
		}
		if(ntiles==10){
		    nueva=try(cut(dat[,vari],labels=paste('D',1:10,sep=''),
		 	quantile(dat[,vari],seq(0,1,0.1),na.rm=TRUE), include.lowest =TRUE),
			silent=TRUE)
		    if(class(nueva)=='try-error'){
		      nueva=try(cut(dat[,vari],labels=paste('D',1:10,sep=''),
		 	quantile(jitter(dat[,vari]),seq(0,1,0.1),na.rm=TRUE), include.lowest =TRUE),
			silent=TRUE)
			cat('',fill=TRUE)
			cat('WARNING. Tienes valores de deciles iguales. Se ha creado la ',fill=TRUE)
			cat('discretizacion a partir del jitter de la variable a discretizar',fill=TRUE)
			cat('',fill=TRUE)
			nulo=percentiles.fnc(dat,vari,percentiles=seq(0,1,0.1))
		    }
		    dat[,new.fac]=nueva
		}
	} # FIN

	# SI ETIQUETAS
    	if(is.na(cortes[1]) & !is.na(etiquetas[1])) {
      	if(is.na(var.out)) new.fac=paste(variable,'.r',sep='') else new.fac=var.out
      	if(ntiles==2) dat[,new.fac]=cut(dat[,vari],labels=etiquetas, quantile(dat[,vari],
			c(0,0.5,1),na.rm=TRUE),include.lowest = T)
      	if(ntiles==3) dat[,new.fac]=cut(dat[,vari],labels=etiquetas, quantile(dat[,vari],
			c(0,0.33,0.66,1),na.rm=TRUE),include.lowest = T)
     	 	if(ntiles==4){
			Q=quantile(dat[,vari],seq(0,1,0.25),na.rm = T)
			dif=length(Q)-length(unique(Q))
			if(dif == 1 & Q[1]==Q[2]) Q[1]=Q[1]-1
			if(dif == 1 & Q[2]==Q[3]) Q[2]=NA
			if(dif == 1 & Q[3]==Q[4]) Q[3]=NA
			if(dif == 1 & Q[4]==Q[5]) Q[5]=Q[5]+1
			dat[,new.fac]=cut(dat[,vari],labels=etiquetas, Q ,include.lowest = T)
		}
    	} # FIN SI ETIQUETAS

	if(!is.na(cortes[1]) & is.na(etiquetas[1])) {
     		if(is.na(var.out)) new.fac=paste(variable,'.r',sep='') else new.fac=var.out
      	dat[,new.fac]=cut(dat[,vari],labels=FALSE, cortes,na.rm=TRUE)
    	}

	if(!is.na(cortes[1]) & !is.na(etiquetas[1])) {
		if(is.na(var.out)) new.fac=paste(variable,'.r',sep='') else new.fac=var.out
      	dat[,new.fac]=cut(dat[,vari],labels=etiquetas, cortes,na.rm=TRUE)
	  if(is.numeric(etiquetas)) dat=transforma.variable.fnc(dat, variable=new.fac, nuevo.tipo='numerica')
    	}

    tabla.n=t(as.matrix(table(dat[,new.fac])))
    rownames(tabla.n)='Freq.'
    tabla.n=t(as.matrix(table(dat[,new.fac])))
    rownames(tabla.n)='Freq.'
    tabla.medias=t(as.matrix(tapply(dat[,vari],list(dat[,new.fac]),function(x)
	mean(x,na.rm=TRUE))))
    row.names(tabla.medias)='Media'
    if(is.na(silente[1])) stem(dat[,variable])
    lista=list(resumen=summary(dat[,variable]),metodo=metodo,tabla.n=tabla.n, tabla.medias=tabla.medias)
    if(is.na(silente[1])) print(lista)
    if(is.na(silente[1])){
        cat('-----------------------------------------------------------------------------',fill=TRUE)
        cat('***  Se ha creado la variable',new.fac,'discretizado de la variable',variable,'***',fill=TRUE)
        cat('-----------------------------------------------------------------------------',fill=TRUE)
    }
  return(dat)
  }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# crea.indice.percentilico.x.factor.fnc(dat, variable='lpsa', ntiles=4,
#		que.factor='age2.r',var.out=NA)
#---------------------------------------------------------------------------
 crea.indice.percentilico.x.factor.fnc=function(datos,variable,ntiles,que.factor,var.out){
	dat=datos
	check.que.factor(que.factor)

	dat$index=1:dim(dat)[1]
	dat_=split(dat,dat[,que.factor])
    	if(!is.na(ntiles[1])) {
		metodo='Cortes Percentilicos'
		if(ntiles > 4 & ntiles < 9){
		  cat('',fill=TRUE)
 		  cat('*** Error. El argumento ntiles solo puede valer 2,3,4 o 10 para los deciles ***',fill=TRUE)
		  stop( )
		}
	}

	lab2=c('< P50','> P50')
	lab3=c('< P33','P33-P66','> P66')
	lab4=c('Q1','Q2','Q3','Q4')
	lab10=paste('D',1:10,sep='')

     vari=variable
	lista=list( )
	for(i in 1:length(dat_)){
		dat=dat_[[i]]
     	if(is.na(var.out)) new.fac=paste(variable,'.r',sep='') else new.fac=var.out
     	if(ntiles==2) dat[,new.fac]=cut(dat[,vari],labels=lab2, quantile(dat[,vari],
			c(0,0.5,1),na.rm=TRUE,include.lowest = T))
     	if(ntiles==3) dat[,new.fac]=cut(dat[,vari],labels=lab3, quantile(dat[,vari],
			c(0,0.33,0.66,1),na.rm=TRUE),include.lowest = T)
     	if(ntiles==4){
			Q=quantile(dat[,vari],seq(0,1,0.25),na.rm = T)
			dif=length(Q)-length(unique(Q))
			if(dif == 1 & Q[1]==Q[2]) Q[1]=Q[1]-1
			if(dif == 1 & Q[2]==Q[3]) Q[2]=NA
			if(dif == 1 & Q[3]==Q[4]) Q[3]=NA
			if(dif == 1 & Q[4]==Q[5]) Q[5]=Q[5]+1
 			dat[,new.fac]=cut(dat[,vari],labels=lab4, Q ,include.lowest = T)
		}
		if(ntiles==10) dat[,new.fac]=cut(dat[,vari],labels=paste('D',1:10,sep=''),
			quantile(dat[,vari],seq(0,1,0.1),na.rm=TRUE), include.lowest =TRUE)
		lista[[i]]=dat
	}
	dat=do.call(rbind,lista)
	dat=dat[order(dat$index),]
    cat('-----------------------------------------------------------------------------------',fill=TRUE)
    cat('*** Se ha creado la varible',new.fac,'discretizado percentilico de la variable: ***',fill=TRUE)
    cat('***',variable,'en cada nivel del factor',que.factor                                ,fill=TRUE)
    cat('-----------------------------------------------------------------------------------',fill=TRUE)
 	tabla=frecuencias.fnc(dat, paste(que.factor,':',new.fac,sep=''),silente=TRUE)
	print(tabla)
	nombres=names(dat)
	col.index=match('index',nombres)
	dat=dat[,-col.index]
 return(dat)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# crea.indice.ordinal.fnc(dat, variable='lpsa',que.factor='age2.r',var.out=NA)
#---------------------------------------------------------------------------
 crea.indice.ordinal.fnc=function(datos,variable,que.factor=NA,var.out,descendente){
	dat=datos
	check.que.factor(que.factor)

	nombres=names(dat)
	# ASIGNA VARIABLE DE SALIDA POR DEFECTO
	if(is.na(var.out[1])) var.out=paste(variable,'.ord',sep='')
	dat$index=1:dim(dat)[1]

	n.todo=dim(dat)[1]
	n.comp=dim(na.omit(dat[,c('index',variable)]))[1]
	if(n.todo-n.comp !=0){
		hay.NA=TRUE
		dat.na=dat[is.na(dat[,variable]),]
		dat.na[,var.out]=NA
		dat=dat[!is.na(dat[,variable]),]
	}else{
		hay.NA=FALSE
	}

	# SI HAY QUE.FACTOR
	if(!is.na(que.factor[1])){
		dat_=split(dat,dat[,que.factor])
		lista=list( )
		for(i in 1:length(dat_)){
			dat=dat_[[i]]
			n.dif=length(unique(dat[,variable]))
			que.frec=data.frame(frecuencias.fnc(dat,variable,silente=T)[[1]]$tabla)
			que.frec$Var1=as.numeric(as.character(que.frec$Var1))
			if(descendente) que.frec$orden=1:n.dif else que.frec$orden=n.dif:1
			dat[,var.out]=NA
			for(i in 1:dim(que.frec)[1])
				dat[abs(dat[,variable] - que.frec$Var1[i])<=0.000001,][,var.out]=que.frec$orden[i]
			lista[[i]]=dat
		}
		dat=do.call(rbind,lista)
		if(!hay.NA){
			dat=dat[order(dat$index),]
			col.index=match('index',names(dat))
			dat=dat[,-col.index]
		}
		cat('*** Se ha creado la variable:',var.out,'discretizado ordinal de la variable:',variable,fill=TRUE)
		cat('*** en cada nivel del factor:',que.factor,fill=TRUE)
		cat('',fill=TRUE)
	# SI NO QUE.FACTOR
	}else{
		n.dif=length(unique(dat[,variable]))
		que.frec=data.frame(frecuencias.fnc(dat,variable,silente=T)[[1]]$tabla)
		que.frec$Var1=as.numeric(as.character(que.frec$Var1))
		if(descendente) que.frec$orden=1:n.dif else que.frec$orden=n.dif:1
		dat[,var.out]=NA
		for(i in 1:dim(que.frec)[1])
			dat[abs(dat[,variable] - que.frec$Var1[i]) <= 0.000001,][,var.out]=que.frec$orden[i]
		if(!hay.NA){
			que.col=match('index',names(dat))
			dat=dat[,-c(que.col)]
		}
		cat('*** Se ha creado la variable:',var.out,'discretizado ordinal de la variable:',variable,fill=TRUE)
		cat('',fill=TRUE)
 	} # FIN
	if(hay.NA){
		dat=rbind(dat.na,dat)
		dat=dat[order(dat$index),]
		col.index=match('index',names(dat))
		dat=dat[,-col.index]
	}
 return(dat)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#
#---------------------------------------------------------------------------
 percentiles.fnc=function(datos, variable=NA, percentiles=NA, que.factor=NA,
		silente=NA, latex=FALSE){
	dat=datos
	check.que.factor(que.factor)

 	if(is.na(silente[1])) crea.cat.fnc('PERCENTILES')

	if(is.numeric(variable)){
		nombres=names(datos)
		variable=nombres[variable]
	}

	# SI VECTOR O DATA.FRAME
	if(!is.data.frame(dat)){
		if(is.na(percentiles[1])) {
			Q=quantile(dat,na.rm=TRUE)
			names(Q)=paste('P.',names(Q),sep='')
 			cat('*** Percentiles de la variable NA ***',fill=TRUE)
			cat('',fill=TRUE)
		}else{
			Q=quantile(dat,percentiles,na.rm)
			names(Q)=paste('P.',names(Q),sep='')
 			cat('*** Percentiles de la variable NA ***',fill=TRUE)
			cat('',fill=TRUE)
		}
	 print(Q)
	 return('')
	}else{
		if(is.na(variable[1])){
			cat('',fill=TRUE)
			cat('*** Error. Debes incluir el argumento variable con el nombre o numero ***',fill=TRUE)
			cat('*** de la variable de la que deseas conocer los percentiles.          ***',fill=TRUE)
			cat("*** Ej. variable='rendimiento'                                        ***",fill=TRUE)
		stop( )
		}
	} # FIN SI VECTOR O DATA.FRAME

	# ERROR SI MAS DE UNA VARIABLE
	if(length(variable) > 1){
		cat('*** Error. El numero maximo de variables debe ser 1 ***',fill=TRUE)
		stop( )
	} # FIN DE ERROR

	# DETECTA NOMBRE DE VARIABLE
	if(is.numeric(variable)){
		nombres=names(dat)
		variable_=nombres[variable]
	}else{
		variable_=variable
	} # FIN DETECTA VAR NAME

	# SI NA QUE.FACTOR
	if(is.na(que.factor[1])){
		# SI NA PERCENTILES
		if(is.na(percentiles[1])){
			Q = quantile(dat[,variable],na.rm=TRUE)
			names(Q)=paste('P.',names(Q),sep='')
 			cat('*** Percentiles de la variable:',variable_,' ***',fill=TRUE)
		}else{
		# SI PERCENTILES
			if(sum(percentiles > 1)){
				cat('*** Error. Los valores del vector de percentiles deben estar entro 0 y 1 ***',fill=TRUE)
				cat('*** Ej: percentiles=c(0.25,0.75,0.95)                                    ***',fill=TRUE)
			stop( )
			}
			Q = quantile(dat[,variable],percentiles,na.rm=TRUE)
			names(Q)=paste('P.',names(Q),sep='')
 			cat('*** Percentiles de la variable:',variable_,' ***',fill=TRUE)
		} # FIN SI PERCENTILES
	# SI QUE.FACTOR
	}else{
		dat_=split(dat,dat[,que.factor])
		if(is.na(percentiles[1])){
			x.factor=lapply(dat_, function(x) {
					Q = quantile(x[,variable],na.rm=TRUE)
					names(Q)=paste('P.',names(Q),sep='')
					return(Q) })
			x.factor=data.frame(do.call(rbind, x.factor))
			row.names(x.factor)=paste(que.factor,row.names(x.factor),sep='.')
			Q=x.factor
 			cat('*** Percentiles de la variable:',variable_,'en cada nivel de:',que.factor,' ***',fill=TRUE)
		}else{
			if(sum(percentiles > 1)){
				cat('*** Error. Los valores del vector de percentiles deben estar entro 0 y 1 ***',fill=TRUE)
				cat('*** Ej: percentiles=c(0.25,0.75,0.95)                                    ***',fill=TRUE)
			stop( )
			}
			x.factor=lapply(dat_, function(x) {
					Q = quantile(x[,variable],percentiles,na.rm=TRUE)
					names(Q)=paste('P.',names(Q),sep='')
					return(Q) })
			x.factor=data.frame(do.call(rbind, x.factor))
			row.names(x.factor)=paste(que.factor,row.names(x.factor),sep='.')
			Q=x.factor
 			cat('*** Percentiles de la variable:',variable_,'en cada nivel de:',que.factor,' ***',fill=TRUE)
		} # FIN SI QUE.FACTOR
	} # FIN SI NA. O NO QUE.FACTOR
 cat('',fill=TRUE)
 print(round(Q,4))
 if(latex & !is.na(que.factor[1])) latex.fnc(round(Q,4))
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# Transforma la unidad una variable a tipo entero, caracter, numerico o factor
#---------------------------------------------------------------------------
 transforma.variable.fnc=function(datos=NA, variable,new.var=NA, nuevo.tipo='numerica',
                                                         formato=NA, silente=FALSE) {
	dat=datos

	if(class(try(is.na(datos)))=='logical') {
		cat('',fill=TRUE)
		crea.cat.fnc('transforma.variable.fnc')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat("  datos = tranforma.variable.fnc(OBrienKaiser, variable='V10',       ",fill=TRUE)
		cat("  		nuevo.tipo='entero' )                                     ",fill=TRUE)
		cat("  datos = tranforma.variable.fnc(OBrienKaiser, variable=1,           ",fill=TRUE)
		cat("  		nuevo.tipo='caracter' )                                     ",fill=TRUE)
		cat("  datos = tranforma.variable.fnc(datos, variable='V12'               ",fill=TRUE)
		cat("  		nuevo.tipo='fecha',formato='%d-%m-%y'))                   ",fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Transforma una variable definida por el usuario desde su clase originaria,', fill=TRUE)
		cat(' a cualquier otra de tipo numerica, entera, factor o fecha.                ', fill=TRUE)
		cat(" ",fill=TRUE)
		cat(" sites.google.com/site/ullrtoolbox/02-manipulacion-de-datos/transforma-variable-fnc", fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		print(argumentos('transforma.variable.fnc'))
	return('help')
	}

	if(is.numeric(variable)) {
		nombres=names(datos)
		variable=nombres[variable]
	}

	# SI TRANSFORMAMOS A FECHA
	if(nuevo.tipo=='fecha' & is.na(formato[1])){
		cat('',fill=TRUE)
		cat("*** Error. Si nuevo.tipo='fecha' debes obligatoriamente incluir ***",fill=TRUE)
		cat("*** el argumento fecha con el formato actual de la misma:       ***",fill=TRUE)
		cat('',fill=TRUE)
		cat("*** Ej1. '1-7-12 18:23'    formato='%d-%m-%y %H:%M'	",fill=TRUE)
		cat("*** Ej2. '1-7-2012 18:23'  formato='%d-%m-%Y %H:%M'	",fill=TRUE)
		cat("*** Ej3. '7-1-12'          formato='%d-%m-%y' 			",fill=TRUE)
		cat("*** Ej4. '7-Ene-12'        FORMATO NO SOPORTADO 			",fill=TRUE)
		cat("*** Ej5. '8/11/11'         formato='%d/%m/%y' 			",fill=TRUE)
		cat("*** Ej6. '8-11-2011'       formato='%d-%m-%Y' 			",fill=TRUE)
		cat("*** Ej7. '11.25.2011'      formato='%m.%d.%Y' 			",fill=TRUE)
		cat("*** Ej8. '11.25.11'        formato='%m.%d.%y' 			",fill=TRUE)
		cat("*** Ej9. '18:23:00'        formato='%H:%M:%S''			",fill=TRUE)
		cat('',fill=TRUE)
		stop( )
	}
	if(nuevo.tipo=='fecha'){
		if(is.factor(datos[,variable])) datos[,variable]=as.character(datos[,variable])
		chivato1=sum(is.na(datos[,variable]))
		dat_=datos
		dat_[,variable]=as.POSIXct(datos[,variable],formato,tz='GMT')
		chivato2=sum(is.na(dat_[,variable]))
		# CHECK TRANSFORMACION
		if(chivato1 != dim(datos)[1] & chivato2 == dim(datos)[1]){
			cat('',fill=TRUE)
			cat("*** Error. Es muy probable que el formato de fecha sea incorrecto ***",fill=TRUE)
			cat("***                 Revisa el formato introducido                 ***",fill=TRUE)
			cat('*** Estos son tus 3 primeros datos:',head(datos[,variable],3),fill=TRUE)
			cat('*** y este el formato que has introducido:',formato,fill=TRUE)
			cat('',fill=TRUE)
			cat('*** Estos ejemplos pueden ayudarte:',fill=TRUE)
			cat("*** Ej1. '1-7-12 18:23'    formato='%d-%m-%y %H:%M'	",fill=TRUE)
			cat("*** Ej2. '1-7-2012 18:23'  formato='%d-%m-%Y %H:%M'	",fill=TRUE)
			cat("*** Ej3. '7-1-12'          formato='%d-%m-%y' 			",fill=TRUE)
			cat("*** Ej4. '7-Ene-12'        FORMATO NO SOPORTADO 			",fill=TRUE)
			cat("*** Ej5. '8/11/11'         formato='%d/%m/%y' 			",fill=TRUE)
			cat("*** Ej6. '8-11-2011'       formato='%d-%m-%Y' 			",fill=TRUE)
			cat("*** Ej7. '11.25.2011'      formato='%m.%d.%Y' 			",fill=TRUE)
			cat("*** Ej8. '11.25.11'        formato='%m.%d.%y' 			",fill=TRUE)
			cat("*** Ej9. '18:23:00'        formato='%H:%M:%S''			",fill=TRUE)
			cat('',fill=TRUE)
			stop( )
		}else{
		        if(!silente){
			cat('',fill=TRUE)
			cat('*** Se ha modificado la clase de la variable',variable,fill=TRUE)
			cat('*** a la nueva clase de fecha (POSIXct)',fill=TRUE)
			cat('*** Estos son tus 3 primeros valores transformados:',fill=TRUE)
			print(head(dat_[,variable],3))
		        }
			datos=dat_
		} # FIN CHECK TRANSFORMACION
	return(datos)
	}
	# FIN SI TRANSFORMAMOS A FECHA

	if(nuevo.tipo=='num') nuevo.tipo='numerica'
	if(nuevo.tipo=='numerico') nuevo.tipo='numerica'
	if(nuevo.tipo=='int') nuevo.tipo='entero'
	if(nuevo.tipo=='Factor') nuevo.tipo='factor'
	if(nuevo.tipo=='chr') nuevo.tipo='character'
	if(nuevo.tipo=='caracter') nuevo.tipo='character'

	clase=class(dat[,variable])

	if(is.na(new.var)) new.var=variable

	if(clase=='character' & nuevo.tipo =='numerica')
		dat[,new.var]=as.numeric(dat[,variable])

	if(clase=='character' & nuevo.tipo =='factor')
		dat[,new.var]=as.factor(dat[,variable])

	if(clase=='factor' & nuevo.tipo =='numerica')
		dat[,new.var]=as.numeric(as.character(dat[,variable]))

	if(clase=='factor' & nuevo.tipo =='character')
		dat[,new.var]=as.character(dat[,variable])
	if(clase=='factor' & nuevo.tipo =='factor')
		dat[,new.var]=as.factor(as.character(dat[,variable]))
	if(clase=='integer' & nuevo.tipo =='factor')
		dat[,new.var]=as.factor(dat[,variable])

	if(clase=='integer' & nuevo.tipo =='numerica')
		dat[,new.var]=as.numeric(dat[,variable])

	if(clase=='numeric' & nuevo.tipo =='entero')
		dat[,new.var]=as.integer(dat[,variable])

	if(clase=='numeric' & nuevo.tipo =='caracter')
		dat[,new.var]=as.character(dat[,variable])

	if(clase=='numeric' & nuevo.tipo =='factor')
		dat[,new.var]=as.factor(dat[,variable])

	if(clase=='numeric') clase='numerica'
	if(clase=='integer') clase='entero'
	if(clase=='character') clase='caracter'
	if(!silente){
         cat('*** Se ha transformado la variable:',variable,'desde su version',clase,'a su version:',nuevo.tipo,'***',fill=TRUE)
	 cat('',fill=TRUE)
	 cat(' Resumen de la variable',new.var,fill=TRUE)
	 print(summary(dat[,new.var]))
	}
  return(dat)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# ordena.por.variables.fnc(dat,que.variables)
#---------------------------------------------------------------------------
 ordena.por.variable.fnc=function(datos,variables,descendente=FALSE,silente=FALSE){
	dat=datos
    n.var=length(variables)
    dat$indice=as.numeric(row.names(dat))
    v=variables
    if(n.var==1) dat=dat[order(dat[,v[1]],decreasing = descendente),]
    if(n.var==2) dat=dat[order(dat[,v[1]],dat[,v[2]],decreasing = descendente),]
    if(n.var==3) dat=dat[order(dat[,v[1]],dat[,v[2]],dat[,v[3]],decreasing = descendente),]
    if(n.var==4) dat=dat[order(dat[,v[1]],dat[,v[2]],dat[,v[3]],dat[,v[4]],decreasing = descendente),]
    if(n.var==5) dat=dat[order(dat[,v[1]],dat[,v[2]],dat[,v[3]],dat[,v[4]],
		    dat[,v[5]],decreasing = descendente),]
	
	if(is.numeric(variables)){
		nombres=names(datos)
		variables=nombres[variables]
	}
	if(!silente & !descendente){
    cat('----------------------------------------------------------------------------------',fill=TRUE)
    cat('***  Se ha ordenado (ascendente) la base de datos por las siguientes variables:***',fill=TRUE)
    cat('***',variables,'                                                         ',fill=TRUE)
    cat('----------------------------------------------------------------------------------',fill=TRUE)
	}
	if(!silente & descendente){
    cat('-----------------------------------------------------------------------------------',fill=TRUE)
    cat('***  Se ha ordenado (descendente) la base de datos por las siguientes variables:***',fill=TRUE)
    cat('***',variables,'                                                         ',fill=TRUE)
    cat('-----------------------------------------------------------------------------------',fill=TRUE)
	}

    if(variables[1]=='indice'){
      nombres=names(dat); col=match('indice',nombres)
      dat=dat[,-col]
    }
    if(!silente) print(head(dat))
 return(dat)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# En sistemas operativos OsX y Linux modifica la codificacion UTF8 a latin1
# lo cual posibilita las tildes, dieresis, etc.
#---------------------------------------------------------------------------
 utf.a.latin.fnc=function(datos){
 	names_=names(datos)
	Encoding(names_)='latin1'
	names(datos)=names_
	check=dime.si.hay.factores.fnc(datos)
	factores=check$factores
	for(i in factores){
 		f1=as.character(datos[,i])
		if(is.factor(datos[,i])){ 
		  niveles=levels(datos[,i])
 		  Encoding(niveles)='latin1'
 		  Encoding(f1)='latin1'
 		  datos[,i]=f1
 		  datos=reordena.factor.fnc(datos, que.factor=i, niveles=niveles,
		     silente=TRUE, hacer.NA=TRUE)
		}else{
 		  f1=as.character(datos[,i])
 		  Encoding(f1)='latin1'
 		  datos[,i]=f1
		}
	}
 return(datos)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# Anova.F1F2.fnc(dat.st,fac.intra,fac.inter,tipo=3)
#---------------------------------------------------------------------------
 Anova.F1F2.fnc=function(datos=NA,fac.intra=NA,fac.inter=NA,tipo=3,fac.intra.item=NA,
	poshoc=NA,color=TRUE){

	if(class(try(is.na(datos)))=='logical') {
		cat('',fill=TRUE)
	    crea.cat.fnc('Anova F1F2 minF')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat('# Anova F1F2 minF A x B 2 x 2 MR                               		',fill=TRUE)
	    cat(" fac.intra=list(mrA=c('a1','a2'), mrB=c('b1','b2'))                    ",fill=TRUE)
		cat("  Anova.F1F2.fnc(datos.apilados, fac.intra=fac.intra)              	",fill=TRUE)
		cat('# Anova F1F2 minF A x B 2 x 2 MR factor B MR por items            		',fill=TRUE)
		cat("  Anova.F1F2.fnc(datos.apilados, fac.intra=fac.intra, fac.intra.item='mrB')",fill=TRUE)
		cat('# Anova F1F2 minF A x B 2 x 2 MR factor B MR por items con poshoc 		',fill=TRUE)
		cat("  Anova.F1F2.fnc(datos.apilados, fac.intra=fac.intra, ",fill=TRUE)
		cat("       fac.intra.item='mrB', poshoc='mrA:mrB')",                        fill=TRUE)
	    cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Lleva a cabo el analisis de la varianza por sujetos, por items, asi ', fill=TRUE)
		cat(' como la minF. Admite todos los modelos de Anova con anidadmiento    ', fill=TRUE)
		cat(' ortogonal con un maximo de 3 factores intergrupo y 3 intra.         ', fill=TRUE)
		cat(' El usuario puede solicitar contrastes poshoc tanto par a par, para  ', fill=TRUE)
		cat(' los factores principales asi como de interaccion. La interaccion    ', fill=TRUE)
		cat(' maxima estimable es triple.                                   	', fill=TRUE)
		cat('',fill=TRUE)
		cat(' Estos son los argumentos de la funcion Anova.F1F2.fnc:         	', fill=TRUE)
		print(argumentos('Anova.F1F2.fnc'))
		cat('---------------------------------------------------------------------', fill=TRUE)
	return('help')
	}
	require(car, quietly=TRUE)

	dat.st=datos
	hay.item=match('item',names(dat.st))
	check.fac.intra.fnc(fac.intra)
	check.factores.fnc(fac.inter,fac.intra)
	check.niveles.fnc(dat.st, fac.inter=fac.inter, fac.intra=fac.intra)

  # CHECK SI HAY ITEM
  if(is.na(hay.item[1])) {
    cat('*** No existe una variable con el nombre de item en la base de datos ***', fill=TRUE)
    stop( )
  }

 	# CHECK SI EXISTEN LOS FACTORES INTER
      if(!is.na(fac.inter[1])) {
		n.inter=length(fac.inter)
		ok.fac.inter=check.fac.inter(dat.st, fac.inter)
		if(length(ok.fac.inter)!=0)
			stop('*** No existe algun o algunos de los factores inter incluidos ***')
		for(j in 1:length(fac.inter)){
		    if(!is.factor(dat.st[,fac.inter[j]])){
			dat.st=transform.variable.fnc(dat.st, fac.inter[j], 
			    nuevo.tipo='factor',silente=TRUE)
			cat('',fill=TRUE)
			cat('*** WARNING. La variable incluida como factor intergrupo',j,'no es factor ***',fill=TRUE)
			cat('*** y ha sido transformado a la clase factor. Revisa tus datos.           ***',fill=TRUE)
			cat('',fill=TRUE)
		    }
		}

	 }else{
		n.inter=0
	 }
	# FIN CHECK SI EXISTEN LOS FACTORES INTER

  if(!is.na(fac.inter[1]) & !is.na(fac.intra[1])) factores=c(fac.inter,names(fac.intra))
  if(!is.na(fac.inter[1]) & is.na(fac.intra[1])) factores=c(fac.inter)
  if(is.na(fac.inter[1]) & !is.na(fac.intra[1])) factores=c(names(fac.intra))
  if(!is.na(fac.inter[1])) n.inter=length(fac.inter) else n.inter=0
  if(!is.na(fac.intra[1])) n.intra=length(names(fac.intra)) else n.intra=0

  if(n.inter==0 & tipo==6) {
	cat('',fill=TRUE)
	cat('*** Error. No tienes factores intergrupo, la suma de cuadrados debe ser Tipo 3 ***',fill=TRUE)
	cat('*** Modifica el argumento tipo a tipo=3                                        ***',fill=TRUE)
	cat('',fill=TRUE)
	stop( )
  }

  # ------------------------------------------------------------------------
  # CONSTRUIMOS X.SUJETOS
  # ------------------------------------------------------------------------
  
  # ------------------------------------------------------------------
  # CREAMOS LA VARIABLE CONDICION A PARTIR DE LOS FACTORES A ANALIZAR
  # ------------------------------------------------------------------
  # Check si algun factor tiene NA como valor y lo elimina
  chivato=list( )
  for(i in 1:length(factores)){
    sal1=dat.st[is.na(dat.st[,factores[i]]),]
    chivato[[i]]=dim(sal1)
    if(dim(sal1)[1]!=0) dat.st=dat.st[!is.na(dat.st[,factores[i]]),]
  }
  names(chivato)=factores
  for(i in 1:length(chivato)){
    if(chivato[[i]][1]!= 0){
      cat('',fill=TRUE)
      cat('*** WARNING. El factor.',factores[i],'tiene NA como valores en algunos registros. ***',fill=TRUE)
      cat('*** Esos registros han sido eliminados antes de realizar el analisis.             ',fill=TRUE)
      cat('',fill=TRUE)
    }
  }   
  if(n.inter!=0 & n.intra==0) dat.st=crea.condicion.fnc(dat.st, fac.inter=fac.inter)
  if(n.inter==0 & n.intra!=0) dat.st=crea.condicion.fnc(dat.st, fac.intra=fac.intra)
  if(n.inter!=0 & n.intra!=0) dat.st=crea.condicion.fnc(dat.st, fac.inter=fac.inter, fac.intra=fac.intra)
  # FIN CREA CONDICION
  # ------------------------------------------------------------------
  
  x.sujeto=with(dat.st,
	aggregate(vd,list(sujeto,condicion),function(x) mean(x,na.rm=TRUE)))
  names(x.sujeto)=c('sujeto','condicion','vd')
  x.sujeto=x.sujeto[order(x.sujeto$sujeto),]
  list.fac=data.frame(do.call(rbind,strsplit(as.character(x.sujeto$condicion),'[:.:]')))
  names(list.fac)=factores
  x.sujeto=cbind(x.sujeto,list.fac)
  if(n.inter !=0){
	for(i in length(fac.inter))
		x.sujeto=reordena.factor.fnc(x.sujeto,que.factor=fac.inter[i],
			niveles=levels(dat.st[,fac.inter[i]]),silente=TRUE)
  }
  if(n.intra !=0){
	name.intra=names(fac.intra)
	for(i in length(name.intra))
		x.sujeto=reordena.factor.fnc(x.sujeto,que.factor=name.intra[i],
			niveles=fac.intra[[i]],silente=TRUE)
  }
  x.sujeto.us=desapila.los.datos.fnc(x.sujeto,fac.intra=fac.intra,
      fac.inter=fac.inter,silente=TRUE)
      
  n.suj_=dim(x.sujeto.us)[1]
  son.na=apply(is.na(x.sujeto.us),2,sum)
  if(sum(n.suj_ %in% son.na) !=0){
	cat('', fill=TRUE)
	cat('*** Error. Parece que hay al menos una condicion experimental con ***',fill=TRUE)
	cat('*** todos los valores NA. Revisa tus datos.                       ***',fill=TRUE)
	cat('', fill=TRUE)
	print(x.sujeto.us)
	stop( )
  }  
  
  if(!is.data.frame(x.sujeto.us)){
	cat('', fill=TRUE)
	cat('*** Error. Parece que hay sujetos con celdilla o celdillas vacias   ***',fill=TRUE)
	cat('*** en alguna condicion experimental. Todos los sujetos deben       ***',fill=TRUE)
	cat('*** obligatoriamente presentar un valor o NA en las JxK condiciones ***',fill=TRUE)
	cat('***                           Revisa tus datos                      ***',fill=TRUE)
	stop( )
  }
  
  anova.x.sujeto=try(Anova.fnc(x.sujeto.us,fac.intra=fac.intra,fac.inter=fac.inter,
				tipo=tipo, grafica=FALSE,col.empieza.mr=1, silente=TRUE),silent=TRUE) 
  if(class(anova.x.sujeto)=='try-error'){
	cat('', fill=TRUE)
	cat('*** Error. Ha habido un problema con los datos por sujeto. ***',fill=TRUE)  
	cat('*** Esta es la matriz que no se ha podido analizar.        ***',fill=TRUE)  
	cat('', fill=TRUE)
	print(x.sujeto.us)
	cat('', fill=TRUE)
	stop()
  }
  apilado.suj=apila.los.datos.fnc(x.sujeto.us, fac.inter=fac.inter, fac.intra=fac.intra,
				col.empieza.mr=1, silente=TRUE)
  
  if(n.inter==1 & n.intra==0) mi.factores=fac.inter
  if(n.inter==0 & n.intra==1) mi.factores=names(fac.intra)
  if(n.inter==0 & n.intra==2) mi.factores=paste(names(fac.intra)[1],':',names(fac.intra)[2],sep='')
  if(n.inter==0 & n.intra==3)
      mi.factores=paste(names(fac.intra)[1],':',names(fac.intra)[2],':',names(fac.intra)[3],sep='')
  if(n.inter==1 & n.intra==1) mi.factores=paste(fac.inter,':',names(fac.intra)[1],sep='')
  if(n.inter==1 & n.intra==2) mi.factores=paste(fac.inter,':',names(fac.intra)[1],':',names(fac.intra)[2],sep='')

  if(n.inter==1 & n.intra==3) mi.factores=paste(fac.inter,':',names(fac.intra)[1],':',names(fac.intra)[2],':',
		names(fac.intra)[3],sep='')
  if(n.inter==3 & n.intra==1) 
      mi.factores=paste(fac.inter[1],':',fac.inter[2],':',fac.inter[3],':',names(fac.intra)[1],sep='')
  if(n.inter==2 & n.intra==1)
       mi.factores=paste(fac.inter[1],':',fac.inter[2],':',names(fac.intra)[1],sep='')
  if(n.inter==2 & n.intra==2)
       mi.factores=paste(fac.inter[1],':',fac.inter[2],':',names(fac.intra)[1],':',names(fac.intra)[2],sep='')
  if(n.inter==2 & n.intra==3)
       mi.factores=paste(fac.inter[1],':',fac.inter[2],':',names(fac.intra)[1],':',names(fac.intra)[2],':',
		names(fac.intra)[3],sep='')
  if(n.inter==3 & n.intra==2)
       mi.factores=paste(fac.inter[1],':',fac.inter[2],':',fac.inter[3],':',names(fac.intra)[1],':',
		names(fac.intra)[2],sep='')
  if(n.inter==3 & n.intra==3)
       mi.factores=paste(fac.inter[1],':',fac.inter[2],':',fac.inter[3],':',names(fac.intra)[1],':',
		names(fac.intra)[2],':',names(fac.intra)[3],sep='')

  # ----------------------------------------------------

  # ----------------------------------------------------
  # CONSTRUIMOS X.ITEMS
  x.item=with(dat.st,
	aggregate(vd,list(item,condicion),function(x) mean(x,na.rm=TRUE)))
  names(x.item)=c('item','condicion','vd')
  list.fac=data.frame(do.call(rbind,strsplit(as.character(x.item$condicion),'[:.:]')))
  names(list.fac)=factores
  x.item=cbind(x.item,list.fac)
  if(n.inter !=0){
	for(i in length(fac.inter))
		x.item=reordena.factor.fnc(x.item,que.factor=fac.inter[i],
			niveles=levels(dat.st[,fac.inter[i]]),silente=TRUE)
  }
  if(n.intra !=0){
	name.intra=names(fac.intra)
	for(i in length(name.intra))
		x.item=reordena.factor.fnc(x.item,que.factor=name.intra[i],
			niveles=fac.intra[[i]],silente=TRUE)
  }
  x.item=x.item[order(x.item$item),]

  # ----------------------------------------------------


  # CHECK SI LOS ITEM PASAN POR TODAS LAS CONDICIONES INTRA
  for(j in 1:n.intra){
    cruce=paste('item',names(fac.intra)[j],sep=':')
    matriz=as.matrix(frecuencias.fnc(dat.st, cruce)$tabla)
    chivato2=match(0,matriz[1,])
    if(is.na(chivato2 & is.na(fac.intra.item[1]))){
      cat('',fill=TRUE)
      cat('*** Error. Al parecer tus items pasan por todos los niveles de algun factor o factores ***',fill=TRUE)
      cat('*** de Medidas repetidas. Incluye el argumento fac.intra.item con el nombre de ese     ***',fill=TRUE)
      cat("*** factor o factores: Ej. fac.intra.item='mrB' o fac.intra.item=c('mrA','mrB').       ***",fill=TRUE)
      cat("*** Esta es la tabla de item x MR. REVISA TU DESIGN                                ***",fill=TRUE)
      cat('',fill=TRUE)
      print(matriz)
      stop( )
    }
  }
  # FIN CHECK SI LOS ITEM PASAN POR TODAS LAS CONDICIONES INTRA				
				
  # NO FAC.INTRA ITEM
  if(is.na(fac.intra.item[1])){
	# SI HAY INTER
  	if(n.inter!=0){
  		new.fac.intra=list( )
  		for(i in 1:n.inter)
  			new.fac.intra[[i]]=levels(x.item[,fac.inter[i]])
  		names(new.fac.intra)=fac.inter
  		new.fac.inter=names(fac.intra)
		new.inter=new.fac.inter; new.intra=new.fac.intra
  		new.x.item=desapila.los.datos.fnc(x.item,fac.intra=new.fac.intra,
  			fac.inter=new.fac.inter,silente=TRUE)
  		anova.x.item=try(Anova.fnc(new.x.item,fac.intra=new.fac.intra,fac.inter=new.fac.inter,
				tipo=tipo, col.empieza.mr=1, grafica=FALSE,silente=TRUE),silent=TRUE)
		if(class(anova.x.item)=='try-error'){
			cat('', fill=TRUE)
			cat('*** Error. Ha habido un problema con los datos por item. ***',fill=TRUE)  
			cat('*** Esta es la matriz que no se ha podido analizar.       ***',fill=TRUE)  
			cat('', fill=TRUE)
			print(new.x.item)
			cat('', fill=TRUE)
			stop()
		}							
		apilado.item=apila.los.datos.fnc(new.x.item, fac.intra=new.fac.intra, 
			fac.inter=new.fac.inter, col.empieza.mr=1, silente=TRUE)
  	}else{
	# SI NO HAY INTER
		new.intra=NA
		new.inter=factores
		anova.x.item=try(Anova.fnc(x.item,fac.intra=NA,fac.inter=factores,vd='vd',
				tipo=3, grafica=FALSE,silente=TRUE),silent=TRUE)
		if(class(anova.x.item)=='try-error'){
			cat('', fill=TRUE)
			cat('*** Error. Ha habido un problema con los datos por item. ***',fill=TRUE)  
			cat('*** Esta es la matriz que no se ha podido analizar.       ***',fill=TRUE)  
			cat('', fill=TRUE)
			print(x.item)
			cat('', fill=TRUE)
			stop()
		}				
		apilado.item=x.item
	}

  # SI FAC.INTRA ITEM
  }else{
	if(n.inter==0) que.tipo=3 else que.tipo=tipo
	que.intra=match(fac.intra.item,names(fac.intra))
	new.intra=fac.intra[que.intra]
	if(length(new.intra)==length(fac.intra)){
		new.inter=NA
	}else{
		new.inter=names(fac.intra[-que.intra])
	}
	if(length(new.intra)==1){
		que.frec=paste('item:',names(new.intra),sep='')
		chivato=as.table(frecuencias.fnc(x.item, que.frec, silente=TRUE)$tabla)
		n.item=dim(chivato)[1]
		nombres=colnames(chivato); filas=rownames(chivato)
		chivato=matrix(chivato)
        chivato=recode(chivato[,1], "0=NA")
		chivato=data.frame(matrix(chivato, nrow=n.item))
		names(chivato)=nombres; row.names(chivato)=filas
		if(dim(na.omit(chivato))[1]==0){
		cat('',fill=TRUE)
		cat('*** Error. El factor:',names(new.intra),'no parece ser de medidas repetidas por item.',fill=TRUE)		
		cat('*** Ningun item presenta valores en mas de un nivel de dicho factor.',fill=TRUE)
		cat('*** Si un mismo item pasa por mas de un nivel de otro factor, este debe aparecer ',fill=TRUE)
		cat('*** repetido en la base de datos original.',fill=TRUE)
		cat('',fill=TRUE)
		stop( )
		}
	}
	if(length(new.intra)==2){
		que.frec=paste('item:',names(new.intra)[1],':',names(new.intra)[2],sep='')
		chivato=data.frame(as.table(frecuencias.fnc(x.item, que.frec, silente=TRUE)$tabla))
		chivato=data.frame(frecuencias.fnc(chivato,'item')[[1]]$tabla)
		n.condi.with=prod(do.call(cbind,lapply(new.intra, function(x) length(x))))
		indice=chivato$Freq == n.condi.with
		chivato=chivato[indice,]
		if(dim(chivato)[1]==0){
			cat('',fill=TRUE)
			cat('*** Error. Los factores:',names(new.intra),'no parecn ser de medidas repetidas por item.',fill=TRUE)		
			cat('*** Ningun item presenta valores en mas de un nivel de dichos factores.',fill=TRUE)
			cat('*** Si un mismo item pasa por mas de un nivel de otro factor, este debe aparecer ',fill=TRUE)
			cat('*** repetido en la base de datos original.',fill=TRUE)
			cat('',fill=TRUE)
			stop( )
		}
	}	
	if(length(new.intra)==3){
		que.frec=paste('item:',names(new.intra)[1],':',names(new.intra)[2],':',
			names(new.intra)[3],sep='')
		chivato=data.frame(as.table(frecuencias.fnc(x.item, que.frec, silente=TRUE)$tabla))
		chivato=data.frame(frecuencias.fnc(chivato,'item')[[1]]$tabla)
		n.condi.with=prod(do.call(cbind,lapply(new.intra, function(x) length(x))))
		indice=chivato$Freq == n.condi.with
		chivato=chivato[indice,]
		if(dim(chivato)[1]==0){
			cat('',fill=TRUE)
			cat('*** Error. Los factores:',names(new.intra),'no parecn ser de medidas repetidas por item.',fill=TRUE)		
			cat('*** Ningun item presenta valores en mas de un nivel de dichos factores.',fill=TRUE)
			cat('*** Si un mismo item pasa por mas de un nivel de otro factor, este debe aparecer ',fill=TRUE)
			cat('*** repetido en la base de datos original.',fill=TRUE)
			cat('',fill=TRUE)
			stop( )
		}
	}	
	
	# pone los factores inter como factores intra del design
	if(n.inter!=0){
		for(j in 1:n.inter){
		  que.inter=fac.inter[j]
		  new.intra[[que.inter]]=levels(dat.st[,que.inter])
		}
	}
  	x.item.us=desapila.los.datos.fnc(x.item,fac.intra=new.intra,
		fac.inter=new.inter, silente=TRUE)
	anova.x.item=try(Anova.fnc(x.item.us,fac.intra=new.intra,fac.inter=new.inter,
		col.empieza.mr=1, tipo=que.tipo, grafica =FALSE,silente=TRUE),silent=TRUE)
	if(class(anova.x.item)=='try-error'){
		cat('', fill=TRUE)
		cat('*** Error. Ha habido un problema con los datos por item. ***',fill=TRUE)  
		cat('*** Esta es la matriz que no se ha podido analizar.      ***',fill=TRUE)  
		cat('', fill=TRUE)
		print(x.item.us)
		cat('', fill=TRUE)
		stop()
	}
	apilado.item=apila.los.datos.fnc(x.item.us, fac.intra=new.intra, 
			fac.inter=new.inter, col.empieza.mr=1, silente=TRUE)	
		
  }

  if(length(factores)==1) n.efectos=1
  if(length(factores)==2) n.efectos=3
  if(length(factores)==3) n.efectos=7
  if(length(factores)==4) n.efectos=15
  if(length(factores)==5) n.efectos=31
  if(length(factores)==6) n.efectos=63

  sink('tmp2.mod.nulo.txt'); print(summary(anova.x.sujeto,multivariate=FALSE)); sink()
  raw.x.suj = scan("tmp2.mod.nulo.txt", sep="\n",what=character(), quiet=TRUE)

  # X.SUJETOS SPLIT.PLOT Y MR
  if(n.intra!=0){
	F=integer()
	df.n=integer( )
	df=integer( )
	efectos=character( )
	for(i in 4:(4+n.efectos-1)){
	  x=	raw.x.suj[i]
	  sal= strsplit(x, '  *')[[1]]
	  efectos[i]=sal[1];
	  df.n[i]=as.numeric(sal[3])
	  F[i]=as.numeric(sal[6]);
	  df[i]=as.numeric(sal[5])
	}
	F=as.numeric(na.omit(F));  df=as.numeric(na.omit(df));
	df.n=as.numeric(na.omit(df.n))
	efectos=as.character(na.omit(efectos))
	efectos=ordena.interac.fnc(efectos)
	tabla.x.sujeto=data.frame(efectos,F,df.n,df,indice=1:n.efectos)
  } # CERRAMOS # X.SUJETOS SPLIT.PLOT Y MR

 # X.ITEMS
  if(is.na(fac.intra.item[1])){
  	if(n.inter==0){
    	sink('tmp.mod.nulo.txt'); print(anova.x.item); sink()
    	raw.x.item = scan("tmp.mod.nulo.txt", sep="\n",what=character(), quiet=TRUE)
		F=integer()
		efectos=character( )
		if(tipo==2) secu=4:(4+n.efectos-1)
		if(tipo==3) secu=5:(5+n.efectos-1)
		for(i in secu){
	  		x=	raw.x.item[i]
	  		sal= strsplit(x, '  *')[[1]]
	  		efectos[i]=sal[1];
	  		F[i]=as.numeric(sal[4]);
		}
	}else{
    	sink('tmp.mod.nulo.txt'); print(summary(anova.x.item,multivariate=FALSE)); sink()
    	raw.x.item = scan("tmp.mod.nulo.txt", sep="\n",what=character(), quiet=TRUE)
		F=integer()
		df=integer()
		efectos=character( )
		secu=4:(4+n.efectos-1)
		for(i in secu){
	  		x=	raw.x.item[i]
	  		sal= strsplit(x, '  *')[[1]]
	  		efectos[i]=sal[1];
	  		F[i]=as.numeric(sal[6]);
	  		df[i]=as.numeric(sal[5])
		}
	}
	F=as.numeric(na.omit(F));	df=as.numeric(na.omit(df));
	efectos=as.character(na.omit(efectos))
	efectos=ordena.interac.fnc(efectos)
	tabla.x.item=data.frame(efectos,F,df)

 }else{ # Si fac.intra se repite por intra factores
    sink('tmp2.mod.nulo.txt'); print(summary(anova.x.item, multivariate=FALSE)); sink()
    raw.x.item = scan("tmp2.mod.nulo.txt", sep="\n",what=character(), quiet=TRUE)
	F=integer()
	df=integer( )
	efectos=character( )
	for(i in 4:(4+n.efectos-1)){
	  x=	raw.x.item[i]
	  sal= strsplit(x, '  *')[[1]]
	  efectos[i]=sal[1];
	  F[i]=as.numeric(sal[6]);
	  df[i]=as.numeric(sal[5])
	}
	F=as.numeric(na.omit(F));  df=as.numeric(na.omit(df));
	df.n=as.numeric(na.omit(df.n))
	efectos=as.character(na.omit(efectos))
	efectos=ordena.interac.fnc(efectos)
	tabla.x.item=data.frame(efectos,F,df)
 }

  # UNIMOS AMBAS TABLAS F
	tabla.F=merge(tabla.x.sujeto,tabla.x.item,by='efectos')
	tabla.F=tabla.F[order(tabla.F$indice),]
     tabla.F=tabla.F[,-5]
	names(tabla.F)=c('efectos','F1','df.n','df.d1','F2','df.d2')
  # CALCULAMOS minF
	minF=minF_B.fnc(tabla.F)

   if(is.na(fac.intra.item[1])){
   	 	crea.cat.fnc('ANOVA x SUJETOS')
  		print(summary(anova.x.sujeto,multivariate=FALSE))
  		crea.cat.fnc('DESCRIPTIVOS x SUJETOS')
  		print(suppressWarnings(descriptivos.fnc(apilado.suj, vd='vd', que.factor=mi.factores,
  			grafica=FALSE, silente=TRUE)))
	   	crea.cat.fnc('ANOVA x ITEMS')
  		if(n.inter==0){
    			print(anova.x.item)
		}else{
			print(summary(anova.x.item,multivariate=FALSE))
		}
  		crea.cat.fnc('DESCRIPTIVOS x ITEMS')
		print(suppressWarnings(descriptivos.fnc(apilado.item, vd='vd',que.factor=mi.factores,
			grafica=FALSE, silente=TRUE)))
   	 	crea.cat.fnc('*** minF ****')
		print(minF)
		cat('#------------------------------------------------------------------',fill=TRUE)
		cat('',fill=TRUE)
 	}else{
    	crea.cat.fnc('ANOVA x SUJETOS')
 		print(summary(anova.x.sujeto,multivariate=FALSE))
  		crea.cat.fnc('DESCRIPTIVOS x SUJETOS')
  		print(suppressWarnings(descriptivos.fnc(apilado.suj, vd='vd', que.factor=mi.factores,
  			grafica=FALSE, silente=TRUE)))
	   	crea.cat.fnc('ANOVA x ITEMS')
		print(summary(anova.x.item,multivariate=FALSE))
  		crea.cat.fnc('DESCRIPTIVOS x ITEMS')
		print(suppressWarnings(descriptivos.fnc(apilado.item, vd='vd',que.factor=mi.factores,
			grafica=FALSE, silente=TRUE)))
   	 	crea.cat.fnc('*** minF ****')
		print(minF)
		cat('#------------------------------------------------------------------',fill=TRUE)
		cat('',fill=TRUE)
     }

  # ANALISIS POSHOC
  if(!is.na(poshoc[1])){
    for(i in 1:length(poshoc)){
      x=poshoc[i]
      tipo=strsplit(x,':')[[1]]
      if(length(tipo)==1) {
        crea.cat.fnc('POSHOC x SUJETOS')
		print(contrastes.poshoc.fnc(apilado.suj, que.factor=x,fac.inter=fac.inter, fac.intra=fac.intra,silente=TRUE))
        crea.cat.fnc('POSHOC x ITEMS')
		print(contrastes.poshoc.fnc(apilado.item, que.factor=x, fac.inter=new.inter, fac.intra=new.intra,silente=TRUE))
      }
      if(length(tipo)==2) {
        crea.cat.fnc('EFECTOS SIMPLES x SUJETOS')
	print(efectos.simples.fnc(apilado.suj,interaccion=x,fac.inter=fac.inter, fac.intra=fac.intra,silente=TRUE))
	crea.cat.fnc('EFECTOS SIMPLES x ITEMS')
	#print(efectos.simples.fnc(x.item,interaccion=x,fac.inter=names(fac.intra), fac.intra=fac.inter,silente=TRUE))
	print(efectos.simples.fnc(apilado.item,interaccion=x,fac.inter=new.inter, fac.intra=new.intra,silente=TRUE))
      }
      if(length(tipo)==3) {
	crea.cat.fnc('INTERACCION TRIPLE x SUJETOS')
	print(interaccion.triple.fnc(apilado.suj,interaccion=x,fac.inter=fac.inter, fac.intra=fac.intra,silente=TRUE,
	    color=color))
	crea.cat.fnc('INTERACCION TRIPLE x ITEMS')
	print(interaccion.triple.fnc(apilado.item,interaccion=x,fac.inter=new.inter, fac.intra=new.intra,silente=TRUE,
	    color=color))
      }
    }
  } # FIN POSHOC
 #try(detach(package:car), silent=TRUE)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# minF.fnc(f=(f1,f2),df.numerador,df.denominador(f1,f2)
#---------------------------------------------------------------------------
  minF.fnc=function(f,df.n,df.d){
  	df.prima=(f[1]+f[2])^2 / ((f[1]^2/df.d[2])+ (f[2]^2/df.d[1]))
  	df.prima=floor(df.prima)
  	minf=(f[1]*f[2])/(f[1]+f[2])
  	p=round(1-pf(minf,df.n,df.prima),5)
  	salida=c(minF=round(minf,3), gl=c(df.n,df.prima), p=p)
  	return(salida)
  }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#
#---------------------------------------------------------------------------
  minF_B.fnc=function(tabla.F){
     df.n=tabla.F$df.n
  	df.prima=with(tabla.F,(F1+F2)^2 / ((F1^2/df.d1)+ (F2^2/df.d2)))
  	df.prima=floor(df.prima)
  	minF=round(with(tabla.F,(F1*F2)/(F1+F2)),3)
  	p=round(1-pf(minF,df.n,df.prima),5)
	salida=data.frame(minF,df.n,df.prima,p)
	row.names(salida)=as.character(tabla.F$efectos)
  return(salida)
  }
#---------------------------------------------------------------------------
  
#---------------------------------------------------------------------------
# Hace NA los valores Inf de una variable numerica
#---------------------------------------------------------------------------
 elimina.infinitos.fnc=function(variable){
    variable[!is.na(variable) & !is.finite(variable)]=NA
 return(variable)
 }
#---------------------------------------------------------------------------
 
#---------------------------------------------------------------------------
# FUNCION: main.effects.poshoc.fnc(datos.agregados,factor,hay.inter)
# los datos agregados se obtienen a partir de la funcion:
# 	aggregate.recoded.var.fnc(all.my.data,what.window,new.factor)
# Esta funcion devuelve los contrastes par a par para la data agregada con los
# nuevos my.factors de recodificacion de los my.channels (new.factor)
#---------------------------------------------------------------------------
 contrastes.poshoc.fnc=function(datos,que.factor,hay.inter=NA,contrastes=NA,vd=NA, fac.inter=NA,
                               fac.intra=NA,silente=FALSE) {
  check.que.factor(que.factor)
  check.fac.intra.fnc(fac.intra)
  check.factores.fnc(fac.inter,fac.intra)
  
  datos.agregados=datos
  
  if(!silente)crea.cat.fnc('CONTRASTES POS-HOC')
  
  # CHECK SI EXISTE QUE.FACTOR
  nombres=names(datos)
  existe=match(que.factor,nombres)
  if(is.na(existe[1])){
    cat('',fill=TRUE)
    cat('*** Error. El factor',que.factor,'no existe en la base de datos introducida ***',fill=TRUE)
    stop( )
  }
  
  # SI VD ES NUMERICA
  if(!is.na(vd[1])) {
    if(is.numeric(vd)) {
      vd=nombres[vd]
      etiqueta.vd=vd
    }else{
      etiqueta.vd=vd
    }
  }  
  # SI NA HAY.INTER
  if(is.na(hay.inter[1])){
    es.inter=match(que.factor, fac.inter)
    es.intra=match(que.factor, names(fac.intra))
    if(!is.na(es.inter[1])) hay.inter=TRUE
    if(!is.na(es.intra[1])) hay.inter=FALSE
  }
  
  nombres.var=names(datos.agregados)
  chivato.vd=match('vd',nombres.var)
  chivato.sujeto=match('sujeto',nombres.var)
  chivato.item=match('item',nombres.var)
  
  # SI HAY ITEM SUSTITUYE NOMBRE ITEM POR SUJETO
  if(is.na(chivato.sujeto) & !is.na(chivato.item)){
    datos.agregados=cambia.nombre.var.fnc(datos.agregados, 'item','sujeto',silente=TRUE)
  }
  
  n.fac=length(que.factor)
  
  if(!is.na(contrastes[1]) & !is.list(contrastes)) {
    if(contrastes!='tendencia'){
      cat('*** Error. No has definido una lista de contrastes. O no has escrito adecuadamenta ***',fill=TRUE)
      cat("*** la palabra 'tendencia' si es un contraste de tendencia lo que deseas.          ***",fill=TRUE)
      cat('',fill=TRUE)
      cat('   *** Contrastes de Tendencia ***',fill=TRUE)
      cat("   *** Ej: contrastes='tendencia' ",fill=TRUE)
      cat('',fill=TRUE)
      cat('   *** Contrastes Ortogonales  ***',fill=TRUE)
      cat("   *** Ej: contrastes=list(AB.vs.C=c(1,1,-2), B.vs.C=c(1,-1,0))", fill=TRUE)
      cat('',fill=TRUE)
      cat('*** 2 contrastes ortogonales (J-1): el primero tratamiento A y B vs el C',fill=TRUE)
      cat('*** el segundo (ortogonal al primero) tratamiento A vs B',fill=TRUE)
      cat('',fill=TRUE)
      cat('*** Esta seria la apariencia de la lista de ejemplo de contrastes ortogonales:     ***',fill=TRUE)
      print(list(AB.vs.C=c(1,1,-2), B.vs.C=c(0,1,-1)) )
      cat('',fill=TRUE)
      stop( )
    }
  }
  
  # COMPRUEBA QUE EXISTE LA VARIABLE VD DECLARADA
  if(!is.na(vd)[1]){
    chivato2.vd=match(vd,nombres.var)
    if(is.na(chivato2.vd[1])){
      cat('*** Error. No existe la variable ',vd,' en la base de datos introducida ***',fill=TRUE)
      stop( )
    }
  }
  
  if(n.fac > 1){
    cat('   *** Error. La funcion constrastes.poshoc.fnc solo admite un factor.    ***',fill=TRUE)
    cat('   *** Si deseas analizar dos o mas factores utiliza la funcion:          ***',fill=TRUE)
    cat('   *** efectos.simples.fnc                                                ***',fill=TRUE)
    cat("   *** Ej. efectos.simples.fnc(mis.datos, interaccion='A:B', hay.inter=c(T,T) ) ",fill=TRUE)
    stop( )
  }
  
  # COMPRUEBA SI HAY VD Y SUJETO EN LA DATA
  if(is.na(vd[1]) & is.na(chivato.vd[1])){
    cat('   *** Error, no has declarado una variable dependiente y tus datos parecen ***',fill=TRUE)
    cat('   *** no estar apilados. Indica una variable dependiente  con vd=           ***',fill=TRUE)
    cat("   *** Ej. vd='altura' ", fill=TRUE)
    stop( )
  }
  if(!is.na(vd[1]) & hay.inter & is.na(chivato.sujeto[1])) {
    vd.original=vd;
    cat(paste('$vd: ',vd.original,sep=''),fill=TRUE)
    cat('',fill=TRUE)
    etiqueta.vd=vd
    nombres.var[match(vd,nombres.var)]='vd'
    names(datos.agregados)=nombres.var
    vd='vd'
    datos.agregados$sujeto=paste('suj',1:dim(datos.agregados)[1],sep='')
  }
  if(is.na(vd[1]) & !is.na(chivato.vd[1]) & is.na(chivato.sujeto[1])){
      datos.agregados$sujeto=paste('suj',1:dim(datos.agregados)[1],sep='')
  }
  
  # FIN DE COMPROBACION VD Y SUJETO
  
  # AGREGA POR QUE.FACTOR
  if(is.na(vd)) etiqueta.vd='vd'
  
  if(vd=='vd' | is.na(vd[1])){
    datos.agregados=
      with(datos.agregados, aggregate(vd,list(sujeto,eval(parse(text=que.factor))),
                                      function(x) mean(x,na.rm=TRUE)))
    
  }else{
    datos.agregados=
      with(datos.agregados, aggregate(datos.agregados[,vd],list(sujeto,eval(parse(text=que.factor))),
                                      function(x) mean(x,na.rm=TRUE)))
  }
  
  datos.agregados$x=elimina.infinitos.fnc(datos.agregados$x)
  colnames(datos.agregados)=c('sujeto',que.factor,'vd')
  J=length(unique(datos.agregados[,que.factor]))
  medias=with(datos.agregados, tapply(vd,eval(parse(text=que.factor)),
                                      function(x) mean(x,na.rm=TRUE)))
  n.condi=with(datos.agregados, tapply(vd,eval(parse(text=que.factor)),length))
  
  # SI PAR A PAR
  if(is.na(contrastes[1])){
    pos.hoc=J*(J-1)/2
    vec=matrix(c(1:J),1,J); vec
    compara=tot.pares.fnc(vec)
    niv=unique(as.character(datos.agregados[,que.factor]))
    etiqueta=etiqueta.fnc(niv,compara)
    if(J == 2) {
      cat('*** El factor solicitado solo tiene dos niveles, solo se imprimen las medias ***',fill=TRUE)
      return(c(mean=medias))
    }
    subgrupos=list( )
    for (i in 1:dim(compara)[1]) {
      subgrupos[[i]]=subset(datos.agregados,
                            datos.agregados[,que.factor] == niv[compara[i,1]] |
                              datos.agregados[,que.factor] == niv[compara[i,2]])
    }
    for(i in 1:length(subgrupos)){
      x=subgrupos[[i]]
      x=reordena.factor.fnc(x, que.factor=que.factor, niveles=niv[compara[i,]], hacer.NA=TRUE,
                            silente=TRUE) 
      subgrupos[[i]]=x
    }
    salida=sapply(subgrupos,function(x) {
      if (hay.inter){
        contr=with(x,try(t.test(vd ~ eval(parse(text=que.factor)),
	  var.equal=FALSE),silent=TRUE))                                                         
      }                          
      if (!hay.inter){
        #x[,que.factor]=factor(as.character(x[,que.factor])) ERROR
        x[,'vd']=recode(x[,'vd'], "'NaN'=NA")
        formula=paste('vd ~',que.factor,sep='')
        new.fac.intra=list( )
        new.fac.intra[[1]]=levels(x[,que.factor])
        names(new.fac.intra)=que.factor
        x.us=desapila.los.datos.fnc(x, fac.intra=new.fac.intra, silente=TRUE)
        contr=with(x.us,try(t.test(x.us[,1],x.us[,2],paired= T,var.equal=FALSE),silent=TRUE))
        if(class(contr)=='try-error'){
          contr$statistic=NA;
          contr$parameter=NA
          contr$p.value=NA
        }
      }
      if(class(contr)=='try-error') {
        res=c(t=NA,df=NA,p=NA)
      }else{
        res=c(contr$statistic, contr$parameter, p=contr$p.value)
      }
      return(res)})
    
    salida=as.data.frame(t(salida))
    salida$r2=salida$t^2/(salida$t^2+salida$df)
    salida$p= round(salida$p,4)
    salida$p.hochberg= p.adjust(salida$p,method='hochberg',n=pos.hoc)
    salida=salida[,c(1,2,4,3,5)]
    row.names(salida)=etiqueta; colnames(salida)=c('t','df','r2','p','p.hochberg')
    salida=list(vd=etiqueta.vd, factor=que.factor,medias=medias, pos.hoc=salida)
  } # FIN SI PAR A PAR
  
  # SI TENDENCIAS
  if(!is.na(contrastes[1]) & contrastes[1]=='tendencia'){
    trend.label=c('Lineal','Cuadratico','Cubico')
    datos.agregados[,que.factor]=ordered(datos.agregados[,que.factor])
    if(hay.inter) {
      mod=lm(eval(parse(text=paste('vd ~',que.factor,sep=''))),dat=datos.agregados)
      res=summary(mod)
      tabla=res$coeff[-1,]
      df=as.numeric(res$fstatistic[3])
      tabla=data.frame(tabla)
      tabla$gl=df
      tabla=tabla[,c(1,2,3,5,4)]
      names(tabla)=c('Valor','errT','t','gl','p.val')
    }else{
      require(nlme, quietly = TRUE)
      mod.lme=lme(eval(parse(text=paste('vd ~',que.factor,sep=''))),
                  random=~ 1|sujeto, dat=datos.agregados)
      tabla=summary(mod.lme)$tTable[-1,]
      colnames(tabla)=c('Valor','errT','gl','t','p.val')
    }
    n.cont=dim(tabla)[1]
    if(n.cont <= 3) {
      row.names(tabla)=paste(que.factor,trend.label[1:n.cont],sep='.')
    }else{
      dif=n.cont-length(trend.label)
      label.2=paste('^',4:(dif+3),sep='')
      row.names(tabla)=paste(que.factor,c(trend.label,label.2),sep='.')
    }
    tabla=round(tabla,4)
    salida=list(vd=etiqueta.vd, factor=que.factor,medias=medias,contrastes=contrastes, contrastes=tabla)
  } # FIN TENDENCIAS
  
  # CONTRASTES ORTOGONALES
  if(is.list(contrastes) | is.matrix(contrastes)){
    # CHEQUEA QUE LA LISTA O MATRIZ DE CONTRASTES SEA CORRECTA (J-1)
    if(is.list(contrastes)) {
      niveles=nlevels(datos[,que.factor])
      n.cont=length(contrastes)
      chivato=data.frame(do.call(rbind,lapply(contrastes,
                                              function(x) length(x)==niveles)))
      names(chivato)='cont'
      cuantos=apply(chivato,2,sum)
      if(cuantos!=(niveles-1)){
        cat(' ',fill=TRUE)
        cat('*** Error. Todos tus contrastes deben tener',niveles,'pesos. ***',fill=TRUE)
        cat('*** Tienes', (niveles-1)-cuantos,'contrastes inadecuados     ***',fill=TRUE)
        cat(' ',fill=TRUE)
        print(contrastes)
        cat(' ',fill=TRUE)
        stop( )
      }
      contrastes=do.call(cbind,contrastes)
    }else{
      n.con=dim(contrastes)[2]
      colnames(contrastes)=paste('cont',1:dim(contrastes)[2],sep='.')
    }
    n.lev=nlevels(datos.agregados[,que.factor])
    if(n.cont != n.lev-1){
      cat(' ',fill=TRUE)
      cat('*** Error, la lista de contrastes ortogonales que has incluido debe contener ***',fill=TRUE)
      cat('*** exactamente J-1(',nlev-1,') contrastes ortogonales entre si.             ***',fill=TRUE)
      cat(' ',fill=TRUE)
      print(contrastes)
      cat(' ',fill=TRUE)
      stop( )
    }
    
    #CHEQUEA CONTRASTES
    sumas=apply(contrastes,2,sum)
    indice=sumas != 0
    if(sum(indice)!=0){
      cat('',fill=TRUE)
      cat('*** Error. La suma de los pesos del contraste',names(sumas[indice]),' ***',fill=TRUE)
      cat('*** no suma cero. Modificalo adecuadamente y repite el procedimiento  ***',fill=TRUE)
      cat('',fill=TRUE)
      print(contrastes)
      cat(' ',fill=TRUE)
      stop( )
    }
    row.names(contrastes)=levels(datos.agregados[,que.factor])
    contrasts(datos.agregados[,que.factor])=contrastes
    
    # CAMINO INTER O INTRA
    if(hay.inter) {
      mod=lm(eval(parse(text=paste('vd ~',que.factor,sep=''))),dat=datos.agregados)
      res=summary(mod)
      SCtotal=sum(anova(mod)['Sum Sq'])
      tabla=res$coeff[-1,]
      df=as.numeric(res$fstatistic[3])
      tabla=data.frame(tabla)
      tabla$gl=df
      tabla=tabla[,c(1,2,3,5,4)]
      colnames(tabla)=c('Valor','errT','t','gl','p.val')
    }else{
      require(nlme, quietly = TRUE)
      mod.lme=lme(eval(parse(text=paste('vd ~',que.factor,sep=''))),
                  random=~ 1|sujeto, dat=datos.agregados)
      tabla=summary(mod.lme)$tTable[-1,]
      colnames(tabla)=c('Valor','errT','gl','t','p.val')
      tabla=data.frame(tabla)
      modelo=eval(parse(text=paste('vd ~',que.factor,
                                   '+Error(sujeto/',que.factor,')',sep='')))
      Sum.Sq=summary(aov(modelo,data=datos))[2]
      SCtotal=sum(unlist(Sum.Sq[[1]])[3:4])
    } # FIN CAMINO INTER-INTRA
    
    # SUMAS CUADRATICAS DE CONTRASTES
    suma.pesos=integer( )
    for(i in 1:dim(contrastes)[2])
      suma.pesos[i]=sum(contrastes[,i]^2/n.condi)
    SCcontr= (medias %*% contrastes)^2 /suma.pesos
    SCinter=sum(SCcontr)
    eta.2 = SCcontr/SCtotal
    
    row.names(tabla)=paste(que.factor,colnames(contrastes),sep='.')
    tabla$Ch=abs(as.numeric(medias%*%contrastes))
    tabla=cbind(tabla,t(eta.2))
    nombres=names(tabla); nombres[7]='eta2'; nombres[6]='abs(Ch)';
    names(tabla)=nombres
    tabla=round(tabla,4)
    tabla=tabla[,c(6,1:5,7)]
    salida=list(vd=etiqueta.vd, factor=que.factor,medias=medias,contrastes=contrastes, contrastes=tabla)
  } # FIN CONTRASTES ORTOGONALES
  try(detach(package:nlme),silent=TRUE)
  return(salida)
}
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# Comprueba que la matriz de contrastes ortogonales es correcta
#---------------------------------------------------------------------------
 check.contrastes.fnc=function(datos,poshoc,fac.inter, fac.intra, contrastes){
 	check.factores.fnc(fac.inter,fac.intra)
	if(contrastes[1] =='tend') contrastes='tendencia'
	if(contrastes[1] =='tendencias') contrastes='tendencia'
	if(contrastes[1] =='tendencia') return(1)

	es.intra=match(poshoc, names(fac.intra))
	if(!is.na(es.intra[1])){
		n.niv=length(fac.intra[[es.intra]])
	}else{
		n.niv=nlevels(datos[,poshoc])
	}
	
	if(!is.list(contrastes)){
		cat('',fill=TRUE)
		cat('*** Error. Los contrastes deben ser una lista conteniendo los J-1 contrastes',fill=TRUE)
		cat('*** de la familia ortogonal deseada. ',fill=TRUE)
		cat('*** Ej: mis.cont=list(a.vs.bc=c(-2,1,1), b.vs.c=c(0,-1,1))',fill=TRUE)
		cat('',fill=TRUE)
		stop()
	}
	if(length(contrastes)!=n.niv-1){
		cat('',fill=TRUE)
		etiq1=paste('*** Error, has incluido el factor ',poshoc,' que tiene ',n.niv,' niveles en el argumento poshoc',sep='')
		etiq2=paste('*** pero la lista de contrastes incluida deberia tener ',n.niv-1,' contrastes y tiene en realidad ',length(contrastes),sep='')
		cat(etiq1,fill=TRUE)
		cat(etiq2,fill=TRUE)
		cat('',fill=TRUE)
		stop()
	}		
		
	n.col=as.integer(do.call(rbind,lapply(contrastes, length)))
	if(!identical(n.col,rep(n.niv,n.niv-1))){
		cat('',fill=TRUE)
		cat('',fill=TRUE)
		cat('*** Error. Los contrastes definidos no tienen todos la misma longitud',fill=TRUE)
		cat('*** Recuerda que los niveles que no se contrastan tendran un peso ',fill=TRUE)
		cat('*** igual a 0.   Ej. B.vs.C=c(0,1,-1) ',fill=TRUE)
		cat('***            Esta es tu lista de contrastes',fill=TRUE)
		cat('',fill=TRUE)
		print(contrastes)
		stop( )
	}

	los.cont=data.frame(cbind(do.call(rbind,contrastes)))
	suma=data.frame(suma=do.call(rbind, lapply(contrastes,sum)))
	los.cont$suma=suma$suma
	if(sum(los.cont$suma != 0) != 0) {
		cat('',fill=TRUE)
		cat('',fill=TRUE)
		cat('*** Error. Revisa los contrastes introducidos. El sumatorio de los pesos ***',fill=TRUE)
		cat('*** de cada contraste debe ser obligatoriamente cero.                    ***',fill=TRUE)
		cat('',fill=TRUE)
		print(los.cont)
		cat('',fill=TRUE)
		stop( )
	}
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# tabla.frec.fnc(datos,variables=c('v1','v2','v3') )
# tabla.frec.fnc(datos,variables='v1:v2:v3')
#---------------------------------------------------------------------------
 frecuencias.fnc=function(datos=NA,variables=NA, prop=FALSE, grafica=FALSE, silente=FALSE,
			como.array=FALSE,lim=NA,angulo=45, latex=FALSE){
	if(class(try(is.na(datos)))=='logical') {
		cat('',fill=TRUE)
		crea.cat.fnc('frecuencias.fnc')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat("  frecuencias.fnc(OBrienKaiser)                                        ",fill=TRUE)
		cat("  frecuencias.fnc(OBrienKaiser, variables=2:8, grafica=T)              ",fill=TRUE)
		cat("  frecuencias.fnc(OBrienKaiser, variables='treatment:gender', prop=T) 	",fill=TRUE)
		cat("  frecuencias.fnc(OBrienKaiser, variables='treatment:gender', 			",fill=TRUE)
		cat("  		grafica=T, angulo=45)     										",fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Genera tabla de frecuencias de las variables definidas por el usuario  ', fill=TRUE)
		cat(' en el argumento variable. Si hacemos T el argumento prop obtendremos   ', fill=TRUE)
		cat(' ademas las tablas de proporciones.                                     ', fill=TRUE)
		cat(' Con el argumento angulo indicamos el angulo de giro que deseamos para  ', fill=TRUE)
		cat(' las etiquetas de las variables en el eje x de la grafica               ', fill=TRUE)
		cat(" ",fill=TRUE)
		cat(" sites.google.com/site/ullrtoolbox/estadisticos-descriptivos/frecuencias-fnc", fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		print(argumentos('frecuencias.fnc'))
	return('help')
	}

	require(lattice,quietly =TRUE)
	if(is.na(lim[1])) lim=0.2 else lim=lim/10
	dat=datos
    if(!silente){
      crea.cat.fnc('TABLA DE FRECUENCIAS')
    } # cierre de silente

    	nombres=names(dat)
	if(is.na(variables[1])) variables=nombres
	if(is.numeric(variables)){
	  if(dim(datos)[2] < max(variables)){
	    cat('',fill=TRUE)
	    cat('*** Error. Has indicado un numero de variables superior a las existentes en la base de datos incluida ***',fill=TRUE)
	    cat('',fill=TRUE)
	    stop( )
	  }
	  variables=nombres[variables]
	}
	#Comprueba si se trata de una o varias variables o por el contrario una tabla
	# cruzada de hasta 3 variables
	v=variables

	# EXISTEN LAS VARIABLES
	if(!is.na(variables[1])){
		chivato=existe.variable.fnc(datos,variables)
		if(chivato$cc !=0){
			cat('',fill=TRUE)
			if(chivato$cc > 1){
			 cat('*** Error. Las variables:',chivato$faltan,'no existen en la base de datos ***',fill=TRUE)
	       	 cat('',fill=TRUE)
			}else{
			 cat('*** Error. La variable:',chivato$faltan,'no existe en la base de datos ***',fill=TRUE)
	       	 cat('',fill=TRUE)
			}
		stop( )
		}
	}
	# FIN SI EXISTEN LAS VARIABLES

	var2=strsplit(v,':')
	n=length(var2)
	if(n==1){
  		if(length(var2[[1]])==1) path=0
  		if(length(var2[[1]])==2) path=1
  		if(length(var2[[1]])==3) path=2
  		if(length(var2[[1]])==4) path=3
		if(length(var2[[1]]) > 4){
		 cat('*** Error. El numero de variables por tabla no puede ser superior a cuatro ***',fill=TRUE)
		 stop( )
		}
	}else{
		path=0
	} # FIN DE CHECK

	if(path==0){
 		lista=list()
 		for (i in 1:length(v)){
  			tabla=with(dat, table(eval(parse(text=variables[i]))))
  			maximo=max(tabla)+0.2*sd(tabla);
  			minimo=min(tabla)
  			if(minimo >=0) minimo=0

			if(grafica){
				suppressWarnings(barplot(as.table(tabla), main= v[i],beside=TRUE,legend=FALSE,
				ylim=c(minimo,maximo), col=1:nrow(as.table(tabla))))
				box( )
				if(i!=length(v)) X11( )
			}
			n.total=sum(tabla)
			if(prop) {
				tabla.prop=prop.table(tabla)
  				lista[[i]]=list(n.total=n.total,tabla=tabla,tabla.prop=tabla.prop)
			}else{
			  	lista[[i]]=list(n.total=n.total,tabla=tabla)
			}
 		}
 		names(lista)=variables
	}else{
		# CREA EL MODELO Y ESTIMA LA TABLA
		v=var2[[1]]
		acumula=v[1]
		for(i in 2:length(v)) acumula=paste(acumula,v[i],sep=',')
		if(como.array){
			modelo=paste('table(',acumula,')',sep='')
		}else{
			modelo=paste('ftable(',acumula,')',sep='')
		}
		tabla=with(dat,eval(parse(text=modelo)))
		if(grafica){
			n.var=length(v)
 			grafi=suppressWarnings(barchart(as.table(tabla), scales = list(x = list("free",rot=angulo)),
				stack=FALSE,horizontal=F, ylab='Frecuencia', xlab=v[1],
     			auto.key = list(title = v[n.var])))
			print(grafi)
		}
		if(prop & !como.array){
  			prop.fila=prop.table(tabla,1)
  			prop.col=prop.table(tabla,2)
			lista=list(variables=variables,tabla=tabla,prop.fila=prop.fila,prop.col=prop.col)
		}else{
			lista=list(variables=variables,tabla=tabla)
		}
		# FIN DE MODELO Y TABLA
	}
 if(latex){
 	nombres=names(lista)
	for(i in 2:length(lista)) {
		print(latex.fnc(as.table(lista[[i]])))
	}
 }
 return(lista)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# interaccion.triple.fnc(datos.apilados,interaccion,hay.inter,pivote,vd)
#---------------------------------------------------------------------------
 interaccion.triple.fnc=function(datos,interaccion,hay.inter=NA,fac.inter=NA, color=TRUE,
    fac.intra=NA, pivote=NA, vd=NA, silente=FALSE, etiqueta=NA, grafica=TRUE){
    datos.agregados=datos
	check.fac.intra.fnc(fac.intra)
	check.factores.fnc(fac.inter,fac.intra)

    if(!silente) crea.cat.fnc('INTERACCION TRIPLE')

    facts=strsplit(interaccion,':');
    fac1=facts[[1]][1]; fac2=facts[[1]][2]; fac3=facts[[1]][3]
    
    nombres=names(datos)
    hay.fac1=match(fac1,nombres); hay.fac2=match(fac2,nombres)
    hay.fac3=match(fac3,nombres)

    if(is.na(hay.fac1[1]) | is.na(hay.fac2[1]) | is.na(hay.fac3[1])){
      cat('',fill=TRUE)
      cat('*** Error. Revisa el nombre de la interaccion introducida:',interaccion,'es incorrecta ***',fill=TRUE)
      cat('*** Algun o algunos de los factores de la interaccion no existen en la base de datos   ***',fill=TRUE)
    stop( )
    }

    # SI NA HAY.INTER
    if(is.na(hay.inter[1])){
      es.inter1=match(fac1, fac.inter);	es.intra1=match(fac1, names(fac.intra))
      es.inter2=match(fac2, fac.inter);	es.intra2=match(fac2, names(fac.intra))
      es.inter3=match(fac3, fac.inter);	es.intra3=match(fac3, names(fac.intra))

      if(is.na(es.inter1[1]) & is.na(es.inter2[1]) & is.na(es.inter3[1])) hay.inter=c(F,F,F)
      if(!is.na(es.inter1[1]) & is.na(es.inter2[1]) & is.na(es.inter3[1])) hay.inter=c(T,F,F)
      if(!is.na(es.inter1[1]) & !is.na(es.inter2[1]) & is.na(es.inter3[1])) hay.inter=c(T,T,F)
      if(!is.na(es.inter1[1]) & is.na(es.inter2[1]) & !is.na(es.inter3[1])) hay.inter=c(T,F,T)
      if(is.na(es.inter1[1]) & !is.na(es.inter2[1]) & !is.na(es.inter3[1])) hay.inter=c(F,T,T)
      if(is.na(es.inter1[1]) & is.na(es.inter2[1]) & !is.na(es.inter3[1])) hay.inter=c(F,F,T)
      if(is.na(es.inter1[1]) & !is.na(es.inter2[1]) & is.na(es.inter3[1])) hay.inter=c(F,T,F)
      if(!is.na(es.inter1[1]) & !is.na(es.inter2[1]) & !is.na(es.inter3[1])) hay.inter=c(T,T,T)
    }

        datos.agregados[,fac1]=factor(datos.agregados[,fac1])
        datos.agregados[,fac2]=factor(datos.agregados[,fac2])
        datos.agregados[,fac3]=factor(datos.agregados[,fac3])
	if(is.na(pivote))	{
		pivote=fac1;
		resto=c(fac2,fac3)
		doble=paste(fac2,':',fac3,sep='')
		hay.inter=hay.inter[2:3]
	}else{
	resto=facts[[1]][-pivote]
	doble=paste(resto[1],resto[2],sep=':')
	hay.inter=hay.inter[-pivote]
	pivote=facts[[1]][pivote]
	}

	# CHECK SI NO EXISTE VD Y vd=NA
	chivato.vd=match('vd',names(datos.agregados))
	if(is.na(chivato.vd) & is.na(vd[1])){
	    cat('',fill=TRUE)
	    cat('*** Error. No tienes una variable de nombre vd en la base de datos y no ',fill=TRUE)
	    cat('*** no has incluido el argumento vd. Incluyelo con el nombre de la vd que',fill=TRUE)
	    cat("*** deseas. Ej. vd='peso' ***",fill=TRUE)
	    cat('',fill=TRUE)
	    stop( )
	}    
	chivato.vdep=match(vd,names(datos.agregados))
	chivato.sujeto=match('sujeto',names(datos.agregados))
	chivato.item=match('item',names(datos.agregados))
	if(is.na(chivato.sujeto) & is.na(chivato.item)){
	    cat('',fill=TRUE)
	    cat('*** Error. No existe la variable sujeto o item en tu base de datos. ',fill=TRUE)
	    cat("*** Creala mediante Ej. datos$sujeto=1:dim(datos)[1]                ' ***",fill=TRUE)
	    cat('',fill=TRUE)
	    stop( )
	}    	
	
	# ADELGAZA LA BASE DE DATOS
	if(is.na(chivato.vd) & !is.na(chivato.vdep) & !is.na(chivato.sujeto) & is.na(chivato.item)) 
	  datos.agregados=datos.agregados[,c(facts[[1]],vd,'sujeto')]
	if(is.na(chivato.vd) & !is.na(chivato.vdep) & !is.na(chivato.item) & is.na(chivato.sujeto)) 
	  datos.agregados=datos.agregados[,c(facts[[1]],vd,'item')]	  
	  
	if(!is.na(chivato.vd) & is.na(chivato.vdep) & !is.na(chivato.sujeto) & is.na(chivato.item)) 
	  datos.agregados=datos.agregados[,c(facts[[1]],'vd','sujeto')]
	  
	if(!is.na(chivato.vd) & is.na(chivato.vdep) & !is.na(chivato.item) & is.na(chivato.sujeto)) 
	  datos.agregados=datos.agregados[,c(facts[[1]],'vd','item')]
	  
	
	mi.lista=list( )
	for (i in 1:nlevels(datos.agregados[,pivote])) {
		mi.lista[[i]]=subset(datos.agregados,
		eval(parse(text=pivote)) == levels(datos.agregados[,pivote])[i],)
	}
	if(is.na(vd[1])){
		rangos=lapply(mi.lista,function(x) with(x,range(tapply(vd,
		 	list(eval(parse(text=resto[1])),eval(parse(text=resto[2]))),
			function(x) mean(x,na.rm=TRUE)))))
		ylim=range(rangos)
  		resultado=lapply(mi.lista,function(x) {
				 titulo=paste('Interaccion: ',doble,
		               ' en ',pivote,': ',unique(as.character(x[,pivote])),sep='')
				 efectos.simples.fnc(x,interaccion=doble,hay.inter=hay.inter,
				    color=color,grafica=grafica,silente=TRUE, 
				    etiqueta=etiqueta, limites=ylim, titulo=titulo)})
	}else{
		rangos=lapply(mi.lista,function(x) with(x,range(tapply(x[,vd],
		 	list(eval(parse(text=resto[1])),eval(parse(text=resto[2]))),
				function(x) mean(x,na.rm=TRUE)))))
		ylim=range(rangos)
  		resultado=lapply(mi.lista,function(x){
				 titulo=paste('Interaccion: ',doble,
		               ' en ',pivote,': ',unique(as.character(x[,pivote])),sep='')
 			      efectos.simples.fnc(x,interaccion=doble,hay.inter=hay.inter,
				  vd=vd, color=color, silente=TRUE, etiqueta=etiqueta,
				  apilados=TRUE,
				  limites=ylim, titulo=titulo, grafica=grafica)})
	}
	names(resultado)=levels(datos.agregados[,pivote])
#	cat('**************************************************************************',fill=TRUE)
#	cat('*** Has solicitado analizar la interaccion triple:',interaccion,fill=TRUE)
#	cat('*** El factor **',pivote,'** actua de pivote',fill=TRUE)
#	cat('*** El numero de orden de las graficas son los niveles de:',pivote,fill=TRUE)
#	cat('***  ',levels(datos.agregados[,pivote]),fill=TRUE)
#	cat('**************************************************************************',fill=TRUE)
 return(resultado)
}
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#
#---------------------------------------------------------------------------
 tabla.porcentajes.fnc=function(tabla){
	tabla=as.matrix(tabla)
	por.fila=round(prop.table(tabla,1),4)
	por.colu=round(prop.table(tabla,2),4)
	por.tabla=round(prop.table(tabla),4)
	tabla.c= cbind(tabla,margin.table(tabla,1))
	tabla.c=	rbind(tabla.c,
		cbind(t(margin.table(tabla,2)),margin.table(tabla)))
 return(list(tabla.original=tabla.c,porcentaje.fila=por.fila,
		porcentaje.columna=por.colu, porcentaje.tabla=por.tabla))
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# prueba.independencia.ji.cuadrado.fnc(dat,variables=c('sexo','zona')
#---------------------------------------------------------------------------
 contraste.ji.cuadrado.fnc=function(datos=NA ,variables=NA,p.pobla=NA){

	if(class(try(is.na(datos)))=='logical') {
		cat('',fill=TRUE)
		crea.cat.fnc('contraste.ji.cuadrado.fnc')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat("  # Bondad de ajuste a Ji cuadrado										",fill=TRUE)
		cat("  contraste.ji.cuadrado.fnc(datos, variables='tipo'),fill=TRUE)"		,fill=TRUE)
		cat(" ",fill=TRUE)
		cat("  # Prueba de Independencia de Ji cuadrado"							,fill=TRUE)
		cat("  contraste.ji.cuadrado.fnc(datos, variables=c('V1','V2'))				",fill=TRUE)
		cat("  contraste.ji.cuadrado.fnc(mi.tabla, variables=c('V1','V2'), ",fill=TRUE)
		cat("  			es.tabla=T)				    								",fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Realiza contraste de bondad de ajuste a ji cuadrado y la prueba de 	', fill=TRUE)
		cat(' independencia de ji cuadrado.											', fill=TRUE)
		cat(" ",fill=TRUE)
		cat(" sites.google.com/site/ullrtoolbox/08---analisis-variables-categoricas/contraste-ji-cuadrado-fnc", fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		print(argumentos('contraste.ji.cuadrado.fnc'))
	return('help')
	}

	dat=datos
	if(is.numeric(variables)){
		nombres=names(dat)
		variables=nombres[variables]
	}

	if(!is.na(variables[1])) {
	  indice=match(variables,names(dat))
	  if(sum(is.na(indice)) > 0){
	    cat('', fill=TRUE)
	    cat('*** Error. No existe la variable', variables[is.na(variables[indice])],'en la base de datos introducida ***',fill=TRUE)
	    stop( )
	  }
	}

        if(is.na(variables[1]) & is.table(dat) ) {
          tabla=dat
        }
        if(is.na(variables[1]) & is.na(p.pobla[1])) {
	  tabla=as.table(as.matrix(dat))
	  n.var=length(dim(tabla))
	  variables_=NULL
	}
	if(is.na(variables[1])){
	  contraste=chisq.test(tabla)
	  tabla=tabla.porcentajes.fnc(tabla)
	  esperadas=contraste$expected
	  matriz=matrix(esperadas)
	  cells=dim(matriz)[1]
	  hecho=TRUE
	}else{
	  variables_=variables
	  hecho=FALSE
	  n.var=length(variables)
	  nombres=names(dat)
	  if(is.numeric(variables)) variables_ = nombres[variables]
	}

	if(n.var==1 | n.var==0){
	  crea.cat.fnc('BONDAD DE AJUSTE A JI CUADRADO')
 	}
	if(n.var==2){
	  crea.cat.fnc('PRUEBA DE INDEPENDENCIA DE VARIABLES')
	  if(!is.null(variables_))
	    {cat('*** Variables:',variables_[1],'y',variables_[2],fill=TRUE); cat('',fill=TRUE)}
	}
    if(n.var==2 & !hecho){
	tabla=with(dat, table(dat[,variables[1]],dat[,variables[2]]))
	contraste=chisq.test(tabla)
	tabla=tabla.porcentajes.fnc(tabla)
 	esperadas=contraste$expected
	matriz=matrix(esperadas)
	cells=dim(matriz)[1]
    }
    if(n.var==1){
       if(is.na(p.pobla[1])){
	categ=length(unique(as.character(dat[,variables[1]])))
	p.pobla=rep(1/categ,categ)
	cat('*** Has solicitado una prueba de bondad de ajuste pero no has incluido el vector ',fill=TRUE)
	cat('*** de probabilidades poblacionales. Se asume que deseas probar que la muestra',fill=TRUE)
	cat('*** pertenece a una poblacion con probabilidades iguales para las categorias',fill=TRUE)
	cat('*** de la variable',variables[1],'.Se asumira como vector poblacional p=c(',p.pobla,')',fill=TRUE)
	cat('*** Si deseas otro, incluyelo como argumento de la funcion.',fill=TRUE)
	cat('*** Ej.: contraste.ji.cuadrado.fnc(datos,variables,p.pobla=c(0.25,0.25,0.50))',fill=TRUE)
	cat(' ',fill=TRUE)
       }
      check=sum(p.pobla)
      if(check != 1) {cat('*** Error. El vector de probabilidades introducido no suma 1 ***',fill=TRUE)
	  stop( )}
      tabla=table(dat[,variables[1]])
      contraste=chisq.test(tabla,p=p.pobla)
      esperadas=contraste$expected
      matriz=matrix(esperadas)
      cells=dim(matriz)[1]
      tabla=list(tabla.original=tabla,frecuencias.esperadas=esperadas)
    }
    supuesto1=((sum(matriz >= 5))*100)/cells
    supuesto2= min(matriz)
    supuesto.1=paste('El ',supuesto1,'% de las celdillas es igual o superior a 5',sep='')
    supuesto.2=paste('El valor esperado minimo es ',supuesto2,sep='')
    sup=rbind(supuesto.1,supuesto.2)
    colnames(sup)='Supuestos'
    p=contraste$p.value
    if(n.var==1){
      if(p > 0.05) deci='No rechazo Ho. La muestra pertenece a la misma poblacion (p > 0.05).'
      if(p <= 0.05 & p > 0.01) deci='Rechazo Ho. La muestra no pertence a la misma poblacion (p < 0.05).'
      if(p <= 0.01 & p > 0.001) deci='Rechazo Ho. La muestra no pertence a la misma poblacion (p < 0.01).'
      if(p <= 0.001 ) deci='Rechazo Ho. La muestra no pertence a la misma poblacion. (p < 0.001).'
      resultado=list(bondad.de.ajuste=contraste, contraste=deci,tablas=tabla,Supuestos=sup)
    }
    if(n.var==2 | n.var==0) {
      if(p > 0.05) deci='No rechazo Ho. Variables independientes (p > 0.05).'
      if(p <= 0.05 & p > 0.01) deci='Rechazo Ho. Variables interdependientes (p < 0.05).'
      if(p <= 0.01 & p > 0.001) deci='Rechazo Ho. Variables interdependientes (p < 0.01).'
      if(p <= 0.001 ) deci='Rechazo Ho. Variables interdependientes (p < 0.001).'
      resultado=list(prueba.independencia=contraste, contraste=deci,Supuestos=sup,tablas=tabla)
    }
 return(resultado)
 }
#---------------------------------------------------------------------------

# -------------------------------------------------------------------------------
#
# -------------------------------------------------------------------------------
 calcula.eta.power.MC.fnc=function(datos,col.empieza.mr,resultado,fac.intra=NA,fac.inter=NA,tipo,
	  vd=NA, covariante=NA,cova.x.vi=NA){

	check.factores.fnc(fac.inter,fac.intra)
	combina3=FALSE	
    if(!is.na(fac.inter[1])) n.inter=length(fac.inter) else n.inter=0
    if(!is.na(fac.intra[1])) n.intra=length(names(fac.intra)) else n.intra=0
	#tab.med=crea.tabla.medias.fnc(datos,fac.inter,fac.intra,col.empieza.mr,vd)
    sink('tmp.mod.nulo.txt')
    if(n.intra==0) print(resultado) else print(summary(resultado,multivariate=FALSE))
    sink()
    raw00 = scan("tmp.mod.nulo.txt", sep="\n",what=character(), quiet=TRUE)

    # COMIENZA BUBLE PARA COMPLETAMENTE ALEATORIO
    if(n.inter <=3 & n.intra ==0){
		n.inter=length(fac.inter)
		if(n.inter==1){ # INTER 1
	    		SCb=integer( )
	    		df=integer( )
	    		if(tipo==3){ # SI TIPO 3
	      		sec=5:6
	      		if(!is.na(covariante[1])) sec=6:7
	      		if(!is.na(covariante[1]) & !is.na(cova.x.vi[1])) sec=c(6,8)
	      		for(i in sec){
					x=raw00[i]
					SCb[i] = as.numeric(strsplit(x, '  *')[[1]][2])
					df[i]=  as.numeric(strsplit(x, '  *')[[1]][3])
	      		}
	    		} # FIN TIPO 3
	    		if(tipo==2){ # SI TIPO 2
	      		sec=4:5
	      		if(!is.na(covariante[1])) sec=5:6
	      		if(!is.na(covariante[1]) & !is.na(cova.x.vi[1])) sec=c(5,7)
	      		for(i in sec){
					x=raw00[i]
					SCb[i] = as.numeric(strsplit(x, '  *')[[1]][2])
					df[i]=  as.numeric(strsplit(x, '  *')[[1]][3])
	      		}
	    		} # FIN TIPO 2
	    		SCb=as.numeric(na.omit(SCb)); SCT=sum(SCb)
	    		df=as.numeric(na.omit(df))[2]
	    		eta.sp=SCb[-2]/SCT
	    		eta.sp=data.frame(eta.sp)
	    		row.names(eta.sp)=paste('eta.',fac.inter[1],sep='')
	    		MCintra=SCb[2]/df
	    		lista=list(eta2=eta.sp,MCintra=MCintra[1],df.w=df)
	} # FIN n.inter 1

	# INTER 2
	if(n.inter==2 & n.intra==0){
	  SCb=integer()
	  df=integer( )
	  if(tipo==3){ # tipo 3
	    secu=5:8; mayor=max(secu)
	    if(!is.na(covariante[1])) secu=6:9
	    if(!is.na(covariante[1]) & !is.na(cova.x.vi[1])) secu=c(6,7,9,10)
	    for(i in secu){
	      x=raw00[i]
	      SCb[i] = as.numeric(strsplit(x, '  *')[[1]][2])
	      df[i]=  as.numeric(strsplit(x, '  *')[[1]][3])
	    }
	  } # fin tipo 3
	  if(tipo==2){ # tipo 2
	    secu=4:7; mayor=max(secu)
	    if(!is.na(covariante[1])) secu=5:8
	    if(!is.na(covariante[1]) & !is.na(cova.x.vi[1])) secu=c(5,6,8,9)
	    for(i in secu){
	      x=raw00[i]
	      SCb[i] = as.numeric(strsplit(x, '  *')[[1]][2])
	      df[i]=  as.numeric(strsplit(x, '  *')[[1]][3])
	    }
	  } # fin tipo 2
	  SCb=as.numeric(na.omit(SCb)); SCT=sum(SCb)
	  df=as.numeric(na.omit(df))
	  eta.sp=SCb[-4]/SCT
	  eta.sp=data.frame(eta.sp)
	  row.names(eta.sp)=c(paste('eta.',fac.inter[1],sep=''),
			paste('eta.',fac.inter[2],sep=''),
			paste('eta.',fac.inter[1],':',fac.inter[2],sep=''))
	  MCintra=SCb[4]/df[4]
	  MCintra=rep(MCintra,3)
	  df=data.frame(df); df[,2]=df[4,1]; names(df)=1:2;
	  lista=list(eta2=eta.sp,MCintra=MCintra[1],df.w=df[4,2])
	} # FIN n.inter==2

	# INTER 3
	if(n.inter==3 & n.intra==0){
	  SCb=integer()
	  df=integer( )
	  if(tipo==3){ # tipo 3
	    secu=5:12; mayor=max(secu)
	    if(!is.na(covariante[1])) secu=7:13
	    if(!is.na(covariante[1]) & !is.na(cova.x.vi[1])) secu=c(6,7,9,10)
	    for(i in secu){
	      x=raw00[i]
	      SCb[i] = as.numeric(strsplit(x, '  *')[[1]][2])
	      df[i]=  as.numeric(strsplit(x, '  *')[[1]][3])
	    }
	  } # fin tipo 3
	  if(tipo==2){ # tipo 2
	    secu=4:11; mayor=max(secu)
	    if(!is.na(covariante[1])) secu=5:12
	    if(!is.na(covariante[1]) & !is.na(cova.x.vi[1])) secu=c(5,6,8,9)
	    for(i in secu){
	      x=raw00[i]
	      SCb[i] = as.numeric(strsplit(x, '  *')[[1]][2])
	      df[i]=  as.numeric(strsplit(x, '  *')[[1]][3])
	    }
	  } # fin tipo 2
	  SCb=as.numeric(na.omit(SCb)); SCT=sum(SCb)
	  df=as.numeric(na.omit(df))
	  eta.sp=round(SCb[-8]/SCT,3); eta.sp=eta.sp[-7]
	  eta.sp=data.frame(eta.sp);
	  row.names(eta.sp)=c(paste('eta.',fac.inter[1],sep=''),
			paste('eta.',fac.inter[2],sep=''),
			paste('eta.',fac.inter[3],sep=''),
			paste('eta.',fac.inter[1],':',fac.inter[2],sep=''),
			paste('eta.',fac.inter[1],':',fac.inter[3],sep=''),
			paste('eta.',fac.inter[2],':',fac.inter[3],sep=''))

	  MCintra=SCb[8]/df[8]
	  MCintra=rep(MCintra,7)
	  df=data.frame(df); df[,2]=df[8,1]; names(df)=1:2;
	  lista=list(eta2=eta.sp,MCintra=MCintra[1],df.w=df[8,2])
	} # FIN n.inter==3

    } # FIN BUBLE CA


    # COMIENZA BUBLE PARA COMPLETAMENTE MR
    if(n.inter ==0 & n.intra <=3){
	n.intra=length(names(fac.intra))
 	if(n.intra==1){
	    x=raw00[4]
	    if(!is.na(covariante[1])) x=raw00[5]
            SCfac = as.numeric(strsplit(x, '  *')[[1]][2])
            SCW = as.numeric(strsplit(x, '  *')[[1]][4])
	    dfw= as.numeric(strsplit(x, '  *')[[1]][5])
	    eta.sp=SCfac/(SCfac+unique(SCW))
            eta.sp=data.frame(eta.sp)
	    row.names(eta.sp)=paste('eta.',names(fac.intra)[1],sep='')
 	    MCintra=SCW/dfw
            MCintra=data.frame(MCintra)
	    MCintra=MCintra$MCintra
	    names(MCintra)=paste('MCw.',names(fac.intra)[1],sep='')
	    lista=list(eta2=eta.sp,MCintra=MCintra,df.w=dfw)
	}
	if(n.intra==2){
	  SCb=integer()
	  SCw=integer()
	  df=integer()
	  secuen=4:6
	  if(!is.na(covariante[1])) secuen=c(5,7,9)
	  for(i in secuen){
	    	x=raw00[i]
		SCb[i] = as.numeric(strsplit(x, '  *')[[1]][2])
	     SCw[i] = as.numeric(strsplit(x, '  *')[[1]][4])
		df[i]= as.numeric(strsplit(x, '  *')[[1]][5])
	  }
	  SCb=as.numeric(na.omit(SCb)); SCw=as.numeric(na.omit(SCw))
	  df=as.numeric(na.omit(df))
	  SCT=sum(SCb)+sum(SCw)
	  SCT=sum(SCb)+sum(unique(SCw))

	  eta.sp=SCb/SCT
	  eta.sp=data.frame(eta.sp)
	  row.names(eta.sp)=c(paste('eta.',names(fac.intra[1]),sep=''),
			paste('eta.',names(fac.intra)[2],sep=''),
			paste('eta.',names(fac.intra)[1],':',names(fac.intra)[2],sep=''))

	  SCTp=apply(cbind(SCb,SCw),1,sum)
	  eta.pr=round(SCb/SCTp,3);
       eta.pr=data.frame(eta.pr)
	  eta=cbind(eta.pr,eta.sp)

	  MCintra=SCw/df
	  MCintra=data.frame(MCintra)
  	  MCintra=MCintra$MCintra
	  names(MCintra)=c(paste('MCw.',names(fac.intra[1]),sep=''),
			paste('MCw.',names(fac.intra)[2],sep=''),
			paste('MCw.',names(fac.intra)[1],':',names(fac.intra)[2],sep=''))
	  lista=list(eta2=eta,MCintra=MCintra,df.w=df)
	} # FIN n.intra==2
	if(n.intra==3){
	  SCb=integer()
	  SCw=integer()
	  df=integer()
	  for(i in 4:10){
	    	x=raw00[i]
		SCb[i] = as.numeric(strsplit(x, '  *')[[1]][2])
	     SCw[i] = as.numeric(strsplit(x, '  *')[[1]][4])
		df[i]= as.numeric(strsplit(x, '  *')[[1]][5])
	  }
	  SCb=as.numeric(na.omit(SCb)); SCw=as.numeric(na.omit(SCw))
	  df=as.numeric(na.omit(df))
	  SCT=sum(SCb)+sum(SCw)
	  SCT=sum(SCb)+sum(unique(SCw))

	  eta.sp=round(SCb/SCT,3)
	  eta.sp=data.frame(eta.sp)
	  row.names(eta.sp)=c(paste('eta.',names(fac.intra[1]),sep=''),
			paste('eta.',names(fac.intra)[2],sep=''),
			paste('eta.',names(fac.intra)[3],sep=''),
			paste('eta.',names(fac.intra)[1],':',names(fac.intra)[2],sep=''),
			paste('eta.',names(fac.intra)[1],':',names(fac.intra)[3],sep=''),
			paste('eta.',names(fac.intra)[2],':',names(fac.intra)[3],sep=''),
			paste('eta.',names(fac.intra)[1],':',names(fac.intra)[2],
				':',names(fac.intra)[3],sep=''))
	  eta.sp=eta.sp[-7,]

	  SCTp=apply(cbind(SCb,SCw),1,sum)
	  eta.pr=round(SCb/SCTp,3); eta.pr=eta.pr[-7]
       eta.pr=data.frame(eta.pr)
	  eta=cbind(eta.pr,eta.sp)

	  MCintra=SCw/df
	  MCintra=data.frame(MCintra)
  	  MCintra=MCintra$MCintra
	  names(MCintra)=c(paste('MCw.',names(fac.intra[1]),sep=''),
			paste('MCw.',names(fac.intra)[2],sep=''),
			paste('MCw.',names(fac.intra)[3],sep=''),
			paste('MCw.',names(fac.intra)[1],':',names(fac.intra)[2],sep=''),
			paste('MCw.',names(fac.intra)[1],':',names(fac.intra)[3],sep=''),
			paste('MCw.',names(fac.intra)[2],':',names(fac.intra)[3],sep=''),
			paste('MCw.',names(fac.intra)[1],':',names(fac.intra)[2],
				':',names(fac.intra)[3],sep=''))
	  lista=list(eta2=eta,MCintra=MCintra,df.w=df)
	} # FIN n.intra==3
   } # FIN BUBLE SOLO INTRA

    # COMIENZA BUCLE SPLIT-PLOT
    if(n.inter==1 & n.intra==1){
	  SCb=integer()
	  SCw=integer()
	  df=integer( )
	  secuen=4:6
	  if(!is.na(covariante[1])) secuen=c(5,6,8)
	  if(!is.na(covariante[1]) & !is.na(cova.x.vi[1])) secuen=c(5,7,9)
	  for(i in secuen){
	    x=raw00[i]
 	    sal= strsplit(x, '  *')[[1]]
	    SCb[i]=as.numeric(sal[2]); SCw[i]=as.numeric(sal[4]);
	    df[i]=as.numeric(sal[5])
	  }
	  SCb=as.numeric(na.omit(SCb)); SCw=as.numeric(na.omit(SCw))
	  df=as.numeric(na.omit(df))
	  SCT=sum(SCb)+sum(SCw)
	  SCT=sum(SCb)+sum(unique(SCw))

	  eta.sp=round(SCb/SCT,3)
	  eta.sp=data.frame(eta.sp)
	  row.names(eta.sp)=c(paste('eta.',fac.inter[1],sep=''),
			paste('eta.',names(fac.intra)[1],sep=''),
			paste('eta.',fac.inter[1],':',names(fac.intra)[1],sep=''))

	  SCTp=apply(cbind(SCb,SCw),1,sum)
	  eta.pr=round(SCb/SCTp,3);
       eta.pr=data.frame(eta.pr)
	  eta=cbind(eta.pr,eta.sp)

	  MCintra=SCw/df
	  MCintra=data.frame(MCintra)
          MCintra=MCintra$MCintra
	  names(MCintra)=c(paste('MCw.',fac.inter[1],sep=''),
			paste('MCw.',names(fac.intra)[1],sep=''),
			paste('MCw.',fac.inter[1],':',names(fac.intra)[1],sep=''))
	 lista=list(eta2=eta,MCintra=MCintra,gl.w=df)
    }
     if(n.inter==2 & n.intra==1){
	  SCb=integer()
	  SCw=integer()
	  df=integer( )
	  secuen=4:10
	  if(tipo==2){
	  	if(!is.na(covariante[1])) secuen=c(5:8,10:12)
	  	if(!is.na(covariante[1]) & !is.na(cova.x.vi[1])) secuen=c(5,6,8,9,11,12,14)
	  }
	  if(tipo==3){
	  	if(!is.na(covariante[1])) secuen=c(5:8,10:12)
	  	if(!is.na(covariante[1]) & !is.na(cova.x.vi[1])) secuen=c(5,6,8,9,11,12,14)
	  }
  
	  for(i in secuen){
	    x=raw00[i]
	    sal= strsplit(x, '  *')[[1]]
	    SCb[i]=as.numeric(sal[2]); SCw[i]=as.numeric(sal[4]);
	    df[i]=as.numeric(sal[5])
	  }
	  SCb=as.numeric(na.omit(SCb));	SCw=as.numeric(na.omit(SCw));  df=as.numeric(na.omit(df));
	  SCT=sum(SCb)+sum(SCw)
	  SCT=sum(SCb)+sum(unique(SCw))
	  eta.sp=round(SCb/SCT,3); eta.sp=eta.sp[-7]
          eta.sp=data.frame(eta.sp)
	  row.names(eta.sp)=c(paste('eta.',fac.inter[1],sep=''),
			paste('eta.',fac.inter[2],sep=''),
			paste('eta.',fac.inter[1],':',fac.inter[2],sep=''),
			paste('eta.',names(fac.intra)[1],sep=''),
			paste('eta.',fac.inter[1],':',names(fac.intra)[1],sep=''),
			paste('eta.',fac.inter[2],':',names(fac.intra)[1],sep=''))

	  SCTp=apply(cbind(SCb,SCw),1,sum)
	  eta.pr=round(SCb/SCTp,3); eta.pr=eta.pr[-7]
       	  eta.pr=data.frame(eta.pr)
	  eta=cbind(eta.pr,eta.sp)

	  MCintra=SCw/df
	  MCintra=data.frame(MCintra)
          MCintra=MCintra$MCintra
	  names(MCintra)=c(paste('MCw.',fac.inter[1],sep=''),
			paste('MCw.',fac.inter[2],sep=''),
			paste('MCw.',fac.inter[1],':',fac.inter[2],sep=''),
			paste('MCw.',names(fac.intra)[1],sep=''),
			paste('MCw.',fac.inter[1],':',names(fac.intra)[1],sep=''),
			paste('MCw.',fac.inter[2],':',names(fac.intra)[1],sep=''),
			paste('MCw.',fac.inter[1],':',fac.inter[2],':',names(fac.intra)[1],sep=''))
	 lista=list(eta2=eta,MCintra=MCintra,gl.w=df)
     }
    if(n.inter==1 & n.intra==2){
	  SCb=integer()
	  SCw=integer()
	  df=integer( )
	  secu=4:10
	  if(!is.na(covariante[1])) secu=c(5,6,8,9,11,12,14)
	  if(!is.na(covariante[1]) & !is.na(cova.x.vi[1])) secu=c(5,7,9,11,13,15,17)
	  for(i in secu){
	    x=raw00[i]
	    sal= strsplit(x, '  *')[[1]]
	    SCb[i]=as.numeric(sal[2]); SCw[i]=as.numeric(sal[4]);
	    df[i]=as.numeric(sal[5])
	  }
	  SCb=as.numeric(na.omit(SCb));	SCw=as.numeric(na.omit(SCw));  df=as.numeric(na.omit(df));
	  SCT=sum(SCb)+sum(SCw)
	  SCT=sum(SCb)+sum(unique(SCw))
	  eta.sp=round(SCb/SCT,3); eta.sp=eta.sp[-7]
		eta.sp=data.frame(eta.sp);
	  row.names(eta.sp)=c(paste('eta.',fac.inter[1],sep=''),
 			paste('eta.',names(fac.intra)[1],sep=''),
			paste('eta.',fac.inter[1],':',names(fac.intra)[1],sep=''),
 			paste('eta.',names(fac.intra)[2],sep=''),
			paste('eta.',fac.inter[1],':',names(fac.intra)[2],sep=''),
			paste('eta.',names(fac.intra)[1],':',names(fac.intra)[2],sep=''))

	  SCTp=apply(cbind(SCb,SCw),1,sum)
	  eta.pr=round(SCb/SCTp,3); eta.pr= eta.pr=eta.pr[-7]
       eta.pr=data.frame(eta.pr)
	  eta=cbind(eta.pr,eta.sp)

	  MCintra=SCw/df; 	 MCintra=MCintra[-7]
	  MCintra=data.frame(MCintra)
	  df=df[-7]
	  MCintra=MCintra$MCintra
	  names(MCintra)=c(paste('MCw.',fac.inter[1],sep=''),
 			paste('MCw.',names(fac.intra)[1],sep=''),
			paste('MCw.',fac.inter[1],':',names(fac.intra)[1],sep=''),
 			paste('MCw.',names(fac.intra)[2],sep=''),
			paste('MCw.',fac.inter[1],':',names(fac.intra)[2],sep=''),
			paste('MCw.',names(fac.intra)[1],':',names(fac.intra)[2],sep=''))
	 lista=list(eta2=eta,MCintra=MCintra,gl.w=df)
     }
    if(n.inter==2 & n.intra==2){
	  SCb=integer()
	  SCw=integer()
	  df=integer( )
	  secu=4:18
	  if(!is.na(covariante[1])) secu=c(5:8,10:13,15:18,20:22)
	  if(!is.na(covariante[1]) & !is.na(cova.x.vi[1])) secu=c(5,6,8,9,11,12,14,15,17,18,20,21,23,24,26)
	  for(i in secu){
	    x=raw00[i]
	    sal= strsplit(x, '  *')[[1]]
	    SCb[i]=as.numeric(sal[2]); SCw[i]=as.numeric(sal[4]);
	    df[i]=as.numeric(sal[5])
	  }
	  SCb=as.numeric(na.omit(SCb));	SCw=as.numeric(na.omit(SCw));  df=as.numeric(na.omit(df));
	  SCT=sum(SCb)+sum(unique(SCw))

	  eta.sp=round(SCb/SCT,3); eta.sp=eta.sp[-c(7,11,13,14,15)]
          eta.sp=data.frame(eta.sp)
          row.names(eta.sp)=c(paste('eta.',fac.inter[1],sep=''),
			paste('eta.',fac.inter[2],sep=''),
			paste('eta.',fac.inter[1],':',fac.inter[2],sep=''),
 			paste('eta.',names(fac.intra)[1],sep=''),
			paste('eta.',fac.inter[1],':',names(fac.intra)[1],sep=''),
			paste('eta.',fac.inter[2],':',names(fac.intra)[1],sep=''),
 			paste('eta.',names(fac.intra)[2],sep=''),
			paste('eta.',fac.inter[1],':',names(fac.intra)[2],sep=''),
			paste('eta.',fac.inter[2],':',names(fac.intra)[2],sep=''),
			paste('eta.',names(fac.intra)[1],':',names(fac.intra)[2],sep=''))

	  SCTp=apply(cbind(SCb,SCw),1,sum)
	  eta.pr=round(SCb/SCTp,3); eta.pr=eta.pr[-c(7,11,13,14,15)]
       eta.pr=data.frame(eta.pr)
	  eta=cbind(eta.pr,eta.sp)

	  MCintra=SCw/df;
	  df=df[-c(7,11,14,15)]
	  MCintra=data.frame(MCintra)
   	  MCintra=MCintra[-c(7,11,14,15),]
          names(MCintra)=c(paste('MCw.',fac.inter[1],sep=''),
			paste('MCw.',fac.inter[2],sep=''),
			paste('MCw.',fac.inter[1],':',fac.inter[2],sep=''),
 			paste('MCw.',names(fac.intra)[1],sep=''),
			paste('MCw.',fac.inter[1],':',names(fac.intra)[1],sep=''),
			paste('MCw.',fac.inter[2],':',names(fac.intra)[1],sep=''),
			paste('MCw.',fac.inter[1],':',fac.inter[2],':',names(fac.intra)[1],sep=''),
 			paste('MCw.',names(fac.intra)[2],sep=''),
			paste('MCw.',fac.inter[1],':',names(fac.intra)[2],sep=''),
			paste('MCw.',fac.inter[2],':',names(fac.intra)[2],sep=''),
			paste('MCw.',names(fac.intra)[1],':',names(fac.intra)[2],sep=''))
	 lista=list(eta2=eta,MCintra=MCintra,gl.w=df)
     }

    	if(n.inter==3 & n.intra==1){
		if(is.na(covariante[1])) { secu=4:18; posiciones=c(1:6,8:11)}
		if(!is.na(covariante[1]) & is.na(cova.x.vi[1]))
			{secu=4:20; posiciones=c(1:6,8,10:12)}
		if(!is.na(covariante[1]) & !is.na(cova.x.vi[1]))
			{secu=4:22; posiciones=c(2:4,6:8,10,12:14)}
	combina3=TRUE
	}	
    	if(n.inter==1 & n.intra==3){
		if(is.na(covariante[1])) { secu=4:18; posiciones=c(1:8,10,12)}
		if(!is.na(covariante[1]) & is.na(cova.x.vi[1]))
			{secu=4:26; posiciones=c(2,3,5,6,8,9,11,12,15,18)}
		if(!is.na(covariante[1]) & !is.na(cova.x.vi[1]))
			{secu=4:34; posiciones=c(2,4,6,8,10,12,14,16,20,24)}
	combina3=TRUE
	}
    	if(n.inter==2 & n.intra==3){
		if(is.na(covariante[1])) { secu=4:34; posiciones=c(1:6,8:10,12,13,14,16,20,24)}
		if(!is.na(covariante[1]) & is.na(cova.x.vi[1]))
			{secu=4:42; posiciones=c(2:5,7,8,10,12,13,15,17,18,20,25,30)}
		if(!is.na(covariante[1]) & !is.na(cova.x.vi[1]))
			{secu=4:50; posiciones=c(2,3,5,6,8,9,12,14,15,18,20,21,24,30,36)}
	combina3=TRUE
	}	
    	if(n.inter==3 & n.intra==2){
		if(is.na(covariante[1])) { secu=4:34; posiciones=c(1:6,8:11,16:19,24)}
		if(!is.na(covariante[1]) & is.na(cova.x.vi[1]))
			{secu=4:38; posiciones=c(2:7,9,11:13,18,20:22,27)}
		if(!is.na(covariante[1]) & !is.na(cova.x.vi[1]))
			{secu=4:42; posiciones=c(2:4,6:8,10,12:14,20,22:24,30 )}
	combina3=TRUE
	}	
    	if(n.inter==3 & n.intra==3){
		if(is.na(covariante[1])) { secu=4:66; posiciones=c(1:6,8:11,16:19,24:27,32,40,48)}
		if(!is.na(covariante[1]) & is.na(cova.x.vi[1]))
			{secu=4:74 ; posiciones=c(2:7,9,11:13,18,20:22,27,29:31,36,45,54)}
		if(!is.na(covariante[1]) & !is.na(cova.x.vi[1]))
			{secu=4:82; posiciones=c(2:4,6:8,10,12:14,20,22:24,30,32:34,40,50,60)}
	combina3=TRUE
	}	
	if(combina3){
	SCb=integer()
  	SCw=integer()
  	df=integer( )
  	etiqueta=character( )
 	for(i in secu){
    		x=raw00[i]
    		sal= strsplit(x, '  *')[[1]]
    		etiqueta[i]=sal[1]
    		SCb[i]=as.numeric(sal[2]); SCw[i]=as.numeric(sal[4]);
    		df[i]=as.numeric(sal[5])
  	}
  	SCb=as.numeric(na.omit(SCb));	SCw=as.numeric(na.omit(SCw));  df=as.numeric(na.omit(df));
  	SCT=sum(SCb)+sum(unique(SCw))
  	etiqueta=na.omit(etiqueta); etiqueta=etiqueta[posiciones]	
  	eta.sp=round(SCb/SCT,3);	 eta.sp=eta.sp[posiciones]
     	eta.sp=data.frame(eta.sp)
     	row.names(eta.sp)=etiqueta
  	SCTp=apply(cbind(SCb,SCw),1,sum)
	eta.pr=round(SCb/SCTp,3); eta.pr=eta.pr[posiciones]
	eta.pr=data.frame(eta.pr)
     	eta=cbind(eta.pr,eta.sp)
  	MCintra=SCw/df;
  	MCintra=MCintra[posiciones]
     	names(MCintra)=etiqueta
     	df=df[posiciones];  names(df)=etiqueta
 	lista=list(eta2=eta,MCintra=MCintra,gl.w=df)
	}
 return(lista)
 }
# -------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
# crea.tabla.medias.fnc(datos,fac.inter,fac.intra=,col.empieza.mr)
# -------------------------------------------------------------------------------
 crea.tabla.medias.fnc=function(datos,fac.inter=NA,fac.intra=NA,col.empieza.mr=NA,vd=NA,
									  covariante=NA){

	check.factores.fnc(fac.inter,fac.intra)

	# CHECK SI EXISTEN LOS FACTORES INTER
      if(!is.na(fac.inter[1])) {
		n.inter=length(fac.inter)
		ok.fac.inter=check.fac.inter(datos, fac.inter)
		if(length(ok.fac.inter)!=0)
			stop('*** No existe algun o algunos de los factores inter incluidos ***')
	 }else{
		n.inter=0
	 }
	# FIN CHECK SI EXISTEN LOS FACTORES INTER

    if(!is.na(fac.intra[1])) n.intra=length(names(fac.intra)) else n.intra=0

    if(n.inter==1 & n.intra==0) {
	if(!is.na(covariante[1])) {
	  new.dat=data.poscovari.fnc(datos, fac.inter=fac.inter, vd=vd, 
		fac.intra=fac.intra, covariante=covariante,
		col.empieza.mr=col.empieza.mr, apilados=F)
	  datos=new.dat
	  datos=cambia.nombre.var.fnc(datos, 'pred','vd',silente=T)
	}    
	media.j=tapply(datos[,vd],datos[,fac.inter],function(x) mean(x,na.rm=TRUE))
	n.j=tapply(datos[,vd],datos[,fac.inter],length)
	t.medias=list(media.j=media.j)
	names(t.medias)=fac.inter
	t.n=list(n.j=n.j)
	names(t.n)=fac.inter
    }
    if(n.inter==2 & n.intra==0) {
	if(!is.na(covariante[1])) {
	  new.dat=data.poscovari.fnc(datos, fac.inter=fac.inter, vd=vd, 
		fac.intra=fac.intra, covariante=covariante,
		col.empieza.mr=col.empieza.mr, apilados=F)
	  datos=new.dat
	  datos=cambia.nombre.var.fnc(datos, 'pred','vd',silente=T)	  
	}
	media.j=tapply(datos[,vd],datos[,fac.inter[1]],function(x) mean(x,na.rm=TRUE))
	media.k=tapply(datos[,vd],datos[,fac.inter[2]],function(x) mean(x,na.rm=TRUE))
	media.jk=tapply(datos[,vd],
		list(datos[,fac.inter[1]],datos[,fac.inter[2]]),function(x)
			mean(x,na.rm=TRUE))
	n.j=tapply(datos[,vd],datos[,fac.inter[1]],length)
	n.k=tapply(datos[,vd],datos[,fac.inter[2]],length)
	n.jk=tapply(datos[,vd],
		list(datos[,fac.inter[1]],datos[,fac.inter[2]]),length)
	t.medias=list(media.j=media.j,media.k=media.k,media.jk=media.jk)
	names(t.medias)=c(fac.inter[1],fac.inter[2],
		paste(fac.inter[1],':',fac.inter[2],sep=''))
	t.n=list(n.j=n.j,n.k=n.k,n.jk=n.jk)
	names(t.n)=c(fac.inter[1],fac.inter[2],
		paste(fac.inter[1],':',fac.inter[2],sep=''))
    }

    if(n.inter==3 & n.intra==0) {
	if(!is.na(covariante[1])) {
	  new.dat=data.poscovari.fnc(datos, fac.inter=fac.inter, vd=vd, 
		fac.intra=fac.intra, covariante=covariante,
		col.empieza.mr=col.empieza.mr, apilados=F)
	  datos=new.dat
	  datos=cambia.nombre.var.fnc(datos, 'pred','vd',silente=T)	  
	}    
	media.j=tapply(datos[,vd],datos[,fac.inter[1]],function(x) mean(x,na.rm=TRUE))
	media.k=tapply(datos[,vd],datos[,fac.inter[2]],function(x) mean(x,na.rm=TRUE))
 	media.l=tapply(datos[,vd],datos[,fac.inter[3]],function(x) mean(x,na.rm=TRUE))

	media.jk=tapply(datos[,vd],
		list(datos[,fac.inter[1]],datos[,fac.inter[2]]),function(x)
			mean(x,na.rm=TRUE))
	media.jl=tapply(datos[,vd],
		list(datos[,fac.inter[1]],datos[,fac.inter[3]]),function(x)
			mean(x,na.rm=TRUE))
	media.kl=tapply(datos[,vd],
		list(datos[,fac.inter[2]],datos[,fac.inter[3]]),function(x)
			mean(x,na.rm=TRUE))

	n.j=tapply(datos[,vd],datos[,fac.inter[1]],length)
	n.k=tapply(datos[,vd],datos[,fac.inter[2]],length)
	n.l=tapply(datos[,vd],datos[,fac.inter[3]],length)

	n.jk=tapply(datos[,vd],
		list(datos[,fac.inter[1]],datos[,fac.inter[2]]),length)
	n.jl=tapply(datos[,vd],
		list(datos[,fac.inter[1]],datos[,fac.inter[3]]),length)
	n.kl=tapply(datos[,vd],
		list(datos[,fac.inter[2]],datos[,fac.inter[3]]),length)

	t.medias=list(media.j=media.j,media.k=media.k,media.l=media.l,
		  media.jk=media.jk, media.jl=media.jl, media.kl=media.kl)
	names(t.medias)=c(fac.inter[1],fac.inter[2],fac.inter[3],
		paste(fac.inter[1],':',fac.inter[2],sep=''),
		paste(fac.inter[1],':',fac.inter[3],sep=''),
		paste(fac.inter[2],':',fac.inter[3],sep=''))

	t.n=list(n.j=n.j,n.k=n.k,n.l=n.l, n.jk=n.jk,n.jl=n.jl,n.kl=n.kl)
	names(t.n)=c(fac.inter[1],fac.inter[2], fac.inter[3],
		paste(fac.inter[1],':',fac.inter[2],sep=''),
		paste(fac.inter[1],':',fac.inter[3],sep=''),
		paste(fac.inter[2],':',fac.inter[3],sep=''))
    }

    if(n.inter==0 & n.intra==1) {
	que.fac=names(fac.intra)
	apilado=apila.los.datos.fnc(datos,fac.intra,col.empieza.mr=col.empieza.mr, silente=TRUE)

	if(!is.na(covariante[1])) {
	  new.dat=data.poscovari.fnc(datos, fac.inter=fac.inter, vd=NA, 
		fac.intra=fac.intra, covariante=covariante,
		col.empieza.mr=col.empieza.mr, apilados=F)
	  new.dat[,'vd']=new.dat$pred
	  apilado=new.dat
	}   	
	
	media.j=tapply(apilado$vd,list(apilado[,que.fac]),function(x) mean(x,na.rm=TRUE))
	n.j=tapply(apilado$vd,list(apilado[,que.fac]),length)
	t.medias=list(media.j=media.j)
	names(t.medias)=que.fac
	t.n=list(n.j=n.j)
	names(t.n)=que.fac
    }
    if(n.inter==0 & n.intra==2) {
	que.fac=names(fac.intra)
	apilado=apila.los.datos.fnc(datos,fac.intra,col.empieza.mr=col.empieza.mr, silente=TRUE)
	if(!is.na(covariante[1])) {
	  new.dat=data.poscovari.fnc(datos, fac.inter=fac.inter, vd=NA, 
		fac.intra=fac.intra, covariante=covariante,
		col.empieza.mr=col.empieza.mr, apilados=F)
	  new.dat[,'vd']=new.dat$pred
	  apilado=new.dat
	}   	
	media.j=tapply(apilado$vd,list(apilado[,que.fac[1]]),function(x) mean(x,na.rm=TRUE))
	media.k=tapply(apilado$vd,list(apilado[,que.fac[2]]),function(x) mean(x,na.rm=TRUE))
	media.jk=tapply(apilado$vd,list(apilado[,que.fac[1]],apilado[,que.fac[2]]),
			function(x) mean(x,na.rm=TRUE))
	n.j=tapply(apilado$vd,list(apilado[,que.fac[1]]),length)
	n.k=tapply(apilado$vd,list(apilado[,que.fac[2]]),length)
	n.jk=tapply(apilado$vd,list(apilado[,que.fac[1]],apilado[,que.fac[2]]),length)
	t.medias=list(media.j=media.j,media.k=media.k,media.jk=media.jk)
	names(t.medias)=c(que.fac[1],que.fac[2],paste(que.fac[1],':',que.fac[2],sep=''))
	t.n=list(n.j=n.j,n.k=n.k,n.jk=n.jk)
	names(t.n)=c(que.fac[1],que.fac[2],paste(que.fac[1],':',que.fac[2],sep=''))
    }

    if(n.inter==0 & n.intra==3) {
	que.fac=names(fac.intra)
	apilado=apila.los.datos.fnc(datos,fac.intra,col.empieza.mr=col.empieza.mr, silente=TRUE)
	if(!is.na(covariante[1])) {
	  new.dat=data.poscovari.fnc(datos, fac.inter=fac.inter, vd=NA, 
		fac.intra=fac.intra, covariante=covariante,
		col.empieza.mr=col.empieza.mr, apilados=F)
	  new.dat[,'vd']=new.dat$pred
	  apilado=new.dat
	}   	

	media.j=tapply(apilado$vd,list(apilado[,que.fac[1]]),function(x) mean(x,na.rm=TRUE))
	media.k=tapply(apilado$vd,list(apilado[,que.fac[2]]),function(x) mean(x,na.rm=TRUE))
	media.l=tapply(apilado$vd,list(apilado[,que.fac[3]]),function(x) mean(x,na.rm=TRUE))
	media.jk=tapply(apilado$vd,list(apilado[,que.fac[1]],apilado[,que.fac[2]]),
			function(x) mean(x,na.rm=TRUE))
	media.jl=tapply(apilado$vd,list(apilado[,que.fac[1]],apilado[,que.fac[3]]),
			function(x) mean(x,na.rm=TRUE))
	media.kl=tapply(apilado$vd,list(apilado[,que.fac[2]],apilado[,que.fac[3]]),
			function(x) mean(x,na.rm=TRUE))
	media.jkl=NA

	n.j=tapply(apilado$vd,list(apilado[,que.fac[1]]),length)
	n.k=tapply(apilado$vd,list(apilado[,que.fac[2]]),length)
	n.l=tapply(apilado$vd,list(apilado[,que.fac[3]]),length)
	n.jk=tapply(apilado$vd,list(apilado[,que.fac[1]],apilado[,que.fac[2]]),length)
	n.jl=tapply(apilado$vd,list(apilado[,que.fac[1]],apilado[,que.fac[3]]),length)
	n.kl=tapply(apilado$vd,list(apilado[,que.fac[2]],apilado[,que.fac[3]]),length)
	n.jkl=NA
	t.medias=list(media.j=media.j,media.k=media.k,media.l=media.l,
		media.jk=media.jk, media.jl=media.jl, media.kl=media.kl)
	names(t.medias)=c(que.fac[1],que.fac[2],que.fac[3],
			paste(que.fac[1],':',que.fac[2],sep=''),
			paste(que.fac[1],':',que.fac[3],sep=''),
  			paste(que.fac[2],':',que.fac[3],sep=''))

	t.n=list(n.j=n.j,n.k=n.k,n.l=n.l,n.jk=n.jk,n.jl=n.jl,n.kl=n.kl)
	names(t.n)=c(que.fac[1],que.fac[2],que.fac[3],
			paste(que.fac[1],':',que.fac[2],sep=''),
			paste(que.fac[1],':',que.fac[3],sep=''),
			paste(que.fac[2],':',que.fac[3],sep=''))
    }

    if(n.inter==1 & n.intra==1) {
	que.fac=names(fac.intra)
	apilado=apila.los.datos.fnc(datos,fac.intra,fac.inter,col.empieza.mr=col.empieza.mr, silente=TRUE)
	if(!is.na(covariante[1])) {
	  new.dat=data.poscovari.fnc(datos, fac.inter=fac.inter, vd=NA, 
		fac.intra=fac.intra, covariante=covariante,
		col.empieza.mr=col.empieza.mr, apilados=F)
	  new.dat[,'vd']=new.dat$pred
	  apilado=new.dat
	}   	

	media.j=tapply(apilado$vd,list(apilado[,fac.inter]),function(x) mean(x,na.rm=TRUE))
	media.k=tapply(apilado$vd,list(apilado[,que.fac[1]]),function(x) mean(x,na.rm=TRUE))
	media.jk=tapply(apilado$vd,list(apilado[,fac.inter],apilado[,que.fac[1]]),
			function(x) mean(x,na.rm=TRUE))
	n.j=tapply(apilado$vd,list(apilado[,fac.inter]),length)
	n.k=tapply(apilado$vd,list(apilado[,que.fac[1]]),length)
	n.jk=tapply(apilado$vd,list(apilado[,fac.inter],apilado[,que.fac[1]]),length)
	t.medias=list(media.j=media.j,media.k=media.k,media.jk=media.jk)
	names(t.medias)=c(fac.inter[1],que.fac[1],
		paste(fac.inter[1],':',que.fac[1],sep=''))
	t.n=list(n.j=n.j,n.k=n.k,n.jk=n.jk)
	names(t.n)=c(fac.inter[1],que.fac[1],
		paste(fac.inter[1],':',que.fac[1],sep=''))
    }
    if(n.inter==2 & n.intra==1) {
	que.fac=names(fac.intra)
	apilado=apila.los.datos.fnc(datos,fac.intra,fac.inter,col.empieza.mr=col.empieza.mr, silente=TRUE)
	if(!is.na(covariante[1])) {
	  new.dat=data.poscovari.fnc(datos, fac.inter=fac.inter, vd=NA, 
		fac.intra=fac.intra, covariante=covariante,
		col.empieza.mr=col.empieza.mr, apilados=F)
	  new.dat[,'vd']=new.dat$pred
	  apilado=new.dat
	}   	

	media.j=tapply(apilado$vd,list(apilado[,fac.inter[1]]),function(x) mean(x,na.rm=TRUE))
	media.k=tapply(apilado$vd,list(apilado[,fac.inter[2]]),function(x) mean(x,na.rm=TRUE))
	media.jk=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,fac.inter[2]]),
			function(x) mean(x,na.rm=TRUE))
	media.l=tapply(apilado$vd,list(apilado[,que.fac]),function(x) mean(x,na.rm=TRUE))
	media.jl=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac]),
			function(x) mean(x,na.rm=TRUE))
	media.kl=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,que.fac]),
			function(x) mean(x,na.rm=TRUE))
	media.jkl=NA
	n.j=tapply(apilado$vd,list(apilado[,fac.inter[1]]),length)
	n.k=tapply(apilado$vd,list(apilado[,fac.inter[2]]),length)
	n.jk=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,fac.inter[2]]),length)
	n.l=tapply(apilado$vd,list(apilado[,que.fac]),length)
	n.jl=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac]),length)
	n.kl=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,que.fac]),length)
	n.jkl=NA
	t.medias=list(media.j=media.j,media.k=media.k,media.jk=media.jk,media.l=media.l,
		media.jl=media.jl,media.kl=media.kl)
	names(t.medias)=c(fac.inter[1],fac.inter[2],
			paste(fac.inter[1],':',fac.inter[2],sep=''),que.fac[1],
			paste(fac.inter[1],':',que.fac[1],sep=''),
			paste(fac.inter[2],':',que.fac[1],sep=''))
	t.n=list(n.j=n.j,n.k=n.k,n.jk=n.jk,n.l=n.l,n.jl=n.jl,n.kl=n.kl)
	names(t.n)=c(fac.inter[1],fac.inter[2],
			paste(fac.inter[1],':',fac.inter[2],sep=''),que.fac[1],
			paste(fac.inter[1],':',que.fac[1],sep=''),
			paste(fac.inter[2],':',que.fac[1],sep=''))
    }
    if(n.inter==1 & n.intra==2) {
	que.fac=names(fac.intra)
	apilado=apila.los.datos.fnc(datos,fac.intra,fac.inter,col.empieza.mr=col.empieza.mr, silente=TRUE)
	if(!is.na(covariante[1])) {
	  new.dat=data.poscovari.fnc(datos, fac.inter=fac.inter, vd=NA, 
		fac.intra=fac.intra, covariante=covariante,
		col.empieza.mr=col.empieza.mr, apilados=F)
	  new.dat[,'vd']=new.dat$pred
	  apilado=new.dat
	}   	

	media.j=tapply(apilado$vd,list(apilado[,fac.inter[1]]),function(x) mean(x,na.rm=TRUE))
	media.k=tapply(apilado$vd,list(apilado[,que.fac[1]]),function(x) mean(x,na.rm=TRUE))
	media.jk=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[1]]),
			function(x) mean(x,na.rm=TRUE))
	media.l=tapply(apilado$vd,list(apilado[,que.fac[2]]),function(x) mean(x,na.rm=TRUE))
	media.jl=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[2]]),
			function(x) mean(x,na.rm=TRUE))
	media.kl=tapply(apilado$vd,list(apilado[,que.fac[1]],apilado[,que.fac[2]]),
			function(x) mean(x,na.rm=TRUE))
	media.jkl=NA
	n.j=tapply(apilado$vd,list(apilado[,fac.inter[1]]),length)
	n.k=tapply(apilado$vd,list(apilado[,que.fac[1]]),length)
	n.jk=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[1]]),length)
	n.l=tapply(apilado$vd,list(apilado[,que.fac[2]]),length)
	n.jl=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[2]]),length)
	n.kl=tapply(apilado$vd,list(apilado[,que.fac[1]],apilado[,que.fac[2]]),length)
	n.jkl=NA
	t.medias=list(media.j=media.j,media.k=media.k,media.jk=media.jk,media.l=media.l,media.jl=media.jl,
		media.kl=media.kl)
	names(t.medias)=c(fac.inter[1],que.fac[1],paste(fac.inter[1],':',que.fac[1],sep=''),
			que.fac[2],paste(fac.inter[1],':',que.fac[2],sep=''),
			paste(que.fac[1],':',que.fac[2],sep=''))
	t.n=list(n.j=n.j,n.k=n.k,n.jk=n.jk,n.l=n.l,n.jl=n.jl,n.kl=n.kl)
	names(t.n)=c(fac.inter[1],que.fac[1],paste(fac.inter[1],':',que.fac[1],sep=''),
			que.fac[2],paste(fac.inter[1],':',que.fac[2],sep=''),
			paste(que.fac[1],':',que.fac[2],sep=''))
    }

    # B2W2
    if(n.inter==2 & n.intra==2) {
	que.fac=names(fac.intra)
	apilado=apila.los.datos.fnc(datos,fac.intra,fac.inter,col.empieza.mr=col.empieza.mr, silente=TRUE)
	if(!is.na(covariante[1])) {
	  new.dat=data.poscovari.fnc(datos, fac.inter=fac.inter, vd=NA, 
		fac.intra=fac.intra, covariante=covariante,
		col.empieza.mr=col.empieza.mr, apilados=F)
	  new.dat[,'vd']=new.dat$pred
	  apilado=new.dat
	}   	

	media.j=tapply(apilado$vd,list(apilado[,fac.inter[1]]),function(x) mean(x,na.rm=TRUE))
	media.k=tapply(apilado$vd,list(apilado[,fac.inter[2]]),function(x) mean(x,na.rm=TRUE))
	media.jk=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,fac.inter[2]]),
			function(x) mean(x,na.rm=TRUE))
	media.l=tapply(apilado$vd,list(apilado[,que.fac[1]]),function(x) mean(x,na.rm=TRUE))
	media.jl=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[1]]),
			function(x) mean(x,na.rm=TRUE))
	media.kl=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,que.fac[1]]),
			function(x) mean(x,na.rm=TRUE))
	media.jkl=NA
	media.m=tapply(apilado$vd,list(apilado[,que.fac[2]]),function(x) mean(x,na.rm=TRUE))
	media.jm=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[2]]),
			function(x) mean(x,na.rm=TRUE))
	media.km=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,que.fac[2]]),
			function(x) mean(x,na.rm=TRUE))
	media.jkm=NA
	media.lm=tapply(apilado$vd,list(apilado[,que.fac[1]],apilado[,que.fac[2]]),
			function(x) mean(x,na.rm=TRUE))
	media.jlm=NA
	media.klm=NA
	media.jklm=NA
	n.j=tapply(apilado$vd,list(apilado[,fac.inter[1]]),length)
	n.k=tapply(apilado$vd,list(apilado[,fac.inter[2]]),length)
	n.jk=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,fac.inter[2]]),length)
	n.l=tapply(apilado$vd,list(apilado[,que.fac[1]]),length)
	n.jl=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[1]]),length)
	n.kl=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,que.fac[1]]),length)
	n.jkl=NA
	n.m=tapply(apilado$vd,list(apilado[,que.fac[2]]),length)
	n.jm=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[2]]),length)
	n.km=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,que.fac[2]]),length)
	n.jkm=NA
	n.lm=tapply(apilado$vd,list(apilado[,que.fac[1]],apilado[,que.fac[2]]),length)
	n.jlm=NA
	n.klm=NA
	n.jklm=NA
	t.medias=list(media.j=media.j,media.k=media.k,media.jk=media.jk,media.l=media.l,
		media.jl=media.jl,media.kl=media.kl,media.m=media.m,media.jm=media.jm,
		media.km=media.km,media.lm=media.lm)
	names(t.medias)=c(fac.inter[1],fac.inter[2],
			paste(fac.inter[1],':',fac.inter[2],sep=''),
			que.fac[1],paste(fac.inter[1],':',que.fac[1],sep=''),
			paste(fac.inter[2],':',que.fac[1],sep=''),
			que.fac[2],paste(fac.inter[1],':',que.fac[2],sep=''),
			paste(fac.inter[2],':',que.fac[2],sep=''),
			paste(que.fac[1],':',que.fac[2],sep=''))
	t.n=list(n.j=n.j,n.k=n.k,n.jk=n.jk,n.l=n.l,n.jl=n.jl,n.kl=n.kl,
		n.m=n.m,n.jm=n.jm,n.km=n.km,n.lm=n.lm)
	names(t.n)=c(fac.inter[1],fac.inter[2],
			paste(fac.inter[1],':',fac.inter[2],sep=''),
			que.fac[1],paste(fac.inter[1],':',que.fac[1],sep=''),
			paste(fac.inter[2],':',que.fac[1],sep=''),
			que.fac[2],paste(fac.inter[1],':',que.fac[2],sep=''),
			paste(fac.inter[2],':',que.fac[2],sep=''),
			paste(que.fac[1],':',que.fac[2],sep=''))
    }
    # FIN B2W2

    # B3W1
    if(n.inter==3 & n.intra==1) {
	que.fac=names(fac.intra)
	apilado=apila.los.datos.fnc(datos,fac.intra,fac.inter,col.empieza.mr=col.empieza.mr, silente=TRUE)
	if(!is.na(covariante[1])) {
	  new.dat=data.poscovari.fnc(datos, fac.inter=fac.inter, vd=NA, 
		fac.intra=fac.intra, covariante=covariante,
		col.empieza.mr=col.empieza.mr, apilados=F)
	  new.dat[,'vd']=new.dat$pred
	  apilado=new.dat
	}   	

	media.j=tapply(apilado$vd,list(apilado[,fac.inter[1]]),function(x) mean(x,na.rm=TRUE))
	media.k=tapply(apilado$vd,list(apilado[,fac.inter[2]]),function(x) mean(x,na.rm=TRUE))
	media.l=tapply(apilado$vd,list(apilado[,fac.inter[3]]),function(x) mean(x,na.rm=TRUE))
	media.jk=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,fac.inter[2]]),
			function(x) mean(x,na.rm=TRUE))
	media.jl=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,fac.inter[3]]),
			function(x) mean(x,na.rm=TRUE))
	media.kl=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,fac.inter[3]]),
			function(x) mean(x,na.rm=TRUE))
	media.jkl=NA
	media.m=tapply(apilado$vd,list(apilado[,que.fac]),function(x) mean(x,na.rm=TRUE))
	media.jl=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac]),
			function(x) mean(x,na.rm=TRUE))
	media.km=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,que.fac]),
			function(x) mean(x,na.rm=TRUE))
	media.lm=tapply(apilado$vd,list(apilado[,fac.inter[3]],apilado[,que.fac]),
			function(x) mean(x,na.rm=TRUE))
	media.jkm=NA
	media.jlm=NA
	media.klm=NA
	media.jklm=NA

	n.j=tapply(apilado$vd,list(apilado[,fac.inter[1]]), length)
	n.k=tapply(apilado$vd,list(apilado[,fac.inter[2]]), length)
	n.l=tapply(apilado$vd,list(apilado[,fac.inter[3]]), length)
	n.jk=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,fac.inter[2]]),length)
	n.jl=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,fac.inter[3]]),length)
	n.kl=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,fac.inter[3]]),length)
	n.jkl=NA
	n.m=tapply(apilado$vd,list(apilado[,que.fac]),function(x) mean(x,na.rm=TRUE))
	n.jl=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac]),length)
	n.km=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,que.fac]),length)
	n.lm=tapply(apilado$vd,list(apilado[,fac.inter[3]],apilado[,que.fac]),length)
	n.jkm=NA
	n.jlm=NA
	n.klm=NA
	n.jklm=NA

	t.medias=list(media.j=media.j,media.k=media.k,media.l=media.l,
		media.jk=media.jk,media.jl=media.jl, media.kl=media.kl, 
		media.m=media.m,
		media.jl=media.jl,media.km=media.km, media.lm=media.lm)
	names(t.medias)=c(fac.inter[1],fac.inter[2],fac.inter[3],
			paste(fac.inter[1],':',fac.inter[2],sep=''),
			paste(fac.inter[1],':',fac.inter[3],sep=''),
			paste(fac.inter[2],':',fac.inter[3],sep=''),
			que.fac[1],
			paste(fac.inter[1],':',que.fac[1],sep=''),
			paste(fac.inter[2],':',que.fac[1],sep=''),
			paste(fac.inter[3],':',que.fac[1],sep=''))

	t.n=list(n.j=n.j,n.k=n.k,n.l=n.l,
		n.jk=n.jk,n.jl=n.jl, n.kl=n.kl, 
		n.m=n.m,
		n.jl=n.jl,n.km=n.km, n.lm=n.lm)
	names(t.n)=c(fac.inter[1],fac.inter[2],fac.inter[3],
			paste(fac.inter[1],':',fac.inter[2],sep=''),
			paste(fac.inter[1],':',fac.inter[3],sep=''),
			paste(fac.inter[2],':',fac.inter[3],sep=''),
			que.fac[1],
			paste(fac.inter[1],':',que.fac[1],sep=''),
			paste(fac.inter[2],':',que.fac[1],sep=''),
			paste(fac.inter[3],':',que.fac[1],sep=''))
    }
    # FIN B3W1

    # B1W3
    if(n.inter==1 & n.intra==3) {
	que.fac=names(fac.intra)
	apilado=apila.los.datos.fnc(datos,fac.intra,fac.inter,col.empieza.mr=col.empieza.mr, silente=TRUE)
	if(!is.na(covariante[1])) {
	  new.dat=data.poscovari.fnc(datos, fac.inter=fac.inter, vd=NA, 
		fac.intra=fac.intra, covariante=covariante,
		col.empieza.mr=col.empieza.mr, apilados=F)
	  new.dat[,'vd']=new.dat$pred
	  apilado=new.dat
	}   	

	media.j=tapply(apilado$vd,list(apilado[,fac.inter[1]]),function(x) mean(x,na.rm=TRUE))
	media.k=tapply(apilado$vd,list(apilado[,que.fac[1]]),function(x) mean(x,na.rm=TRUE))
	media.jk=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[1]]),
			function(x) mean(x,na.rm=TRUE))
	media.l=tapply(apilado$vd,list(apilado[,que.fac[2]]),function(x) mean(x,na.rm=TRUE))
	media.jl=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[2]]),
			function(x) mean(x,na.rm=TRUE))
	media.m=tapply(apilado$vd,list(apilado[,que.fac[3]]),function(x) mean(x,na.rm=TRUE))
	media.jm=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[3]]),
			function(x) mean(x,na.rm=TRUE))
	media.kl=tapply(apilado$vd,list(apilado[,que.fac[1]],apilado[,que.fac[2]]),
			function(x) mean(x,na.rm=TRUE))
	media.jkl=NA
	media.km=tapply(apilado$vd,list(apilado[,que.fac[1]],apilado[,que.fac[3]]),
			function(x) mean(x,na.rm=TRUE))
	media.jkm=NA
	media.lm=tapply(apilado$vd,list(apilado[,que.fac[2]],apilado[,que.fac[3]]),
			function(x) mean(x,na.rm=TRUE))
	media.jlm=NA
	media.klm=NA
	media.jklm=NA

	t.medias=list(media.j=media.j,media.k=media.k,media.jk=media.jk,media.l=media.l,media.jl=media.jl,
		media.m=media.m, media.jm=media.jm, media.kl=media.kl, media.km=media.km,media.lm=media.lm)

	names(t.medias)=c(fac.inter[1],que.fac[1],paste(fac.inter[1],':',que.fac[1],sep=''),
			que.fac[2],paste(fac.inter[1],':',que.fac[2],sep=''), que.fac[3],
			paste(fac.inter[1],':',que.fac[3],sep=''),
			paste(que.fac[1],':',que.fac[2],sep=''),
			paste(que.fac[1],':',que.fac[3],sep=''),
			paste(que.fac[2],':',que.fac[3],sep=''))

	n.j=tapply(apilado$vd,list(apilado[,fac.inter[1]]),length)
	n.k=tapply(apilado$vd,list(apilado[,que.fac[1]]),length)
	n.jk=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[1]]),
			length)
	n.l=tapply(apilado$vd,list(apilado[,que.fac[2]]),length)
	n.jl=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[2]]),
			length)
	n.m=tapply(apilado$vd,list(apilado[,que.fac[3]]),length)
	n.jm=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[3]]),
			length)
	n.kl=tapply(apilado$vd,list(apilado[,que.fac[1]],apilado[,que.fac[2]]),
			length)
	n.jkl=NA
	n.km=tapply(apilado$vd,list(apilado[,que.fac[1]],apilado[,que.fac[3]]),
			length)
	n.jkm=NA
	n.lm=tapply(apilado$vd,list(apilado[,que.fac[2]],apilado[,que.fac[3]]),
			length)
	n.jlm=NA
	n.klm=NA
	n.jklm=NA

	t.n=list(n.j=n.j,n.k=n.k,n.jk=n.jk,n.l=n.l,n.jl=n.jl,
		n.m=n.m, n.jm=n.jm, n.kl=n.kl, n.km=n.km,n.lm=n.lm)

	names(t.n)=c(fac.inter[1],que.fac[1],paste(fac.inter[1],':',que.fac[1],sep=''),
			que.fac[2],paste(fac.inter[1],':',que.fac[2],sep=''), que.fac[3],
			paste(fac.inter[1],':',que.fac[3],sep=''),
			paste(que.fac[1],':',que.fac[2],sep=''),
			paste(que.fac[1],':',que.fac[3],sep=''),
			paste(que.fac[2],':',que.fac[3],sep=''))
    }
    # FIN B1W3

    # B2W3
    if(n.inter==2 & n.intra==3) {
	que.fac=names(fac.intra)
	apilado=apila.los.datos.fnc(datos,fac.intra,fac.inter,col.empieza.mr=col.empieza.mr, silente=TRUE)
	if(!is.na(covariante[1])) {
	  new.dat=data.poscovari.fnc(datos, fac.inter=fac.inter, vd=NA, 
		fac.intra=fac.intra, covariante=covariante,
		col.empieza.mr=col.empieza.mr, apilados=F)
	  new.dat[,'vd']=new.dat$pred
	  apilado=new.dat
	}   	

	media.j=tapply(apilado$vd,list(apilado[,fac.inter[1]]),function(x) mean(x,na.rm=TRUE))
	media.k=tapply(apilado$vd,list(apilado[,fac.inter[2]]),function(x) mean(x,na.rm=TRUE))
	media.jk=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,fac.inter[2]]),
			function(x) mean(x,na.rm=TRUE))
	media.l=tapply(apilado$vd,list(apilado[,que.fac[1]]),function(x) mean(x,na.rm=TRUE))
	media.jl=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[1]]),
			function(x) mean(x,na.rm=TRUE))
	media.kl=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,que.fac[1]]),
			function(x) mean(x,na.rm=TRUE))
	media.jkl=NA
	media.m=tapply(apilado$vd,list(apilado[,que.fac[2]]),function(x) mean(x,na.rm=TRUE))
	media.jm=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[2]]),
			function(x) mean(x,na.rm=TRUE))
	media.km=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,que.fac[2]]),
			function(x) mean(x,na.rm=TRUE))
	media.jkm=NA
	media.n=tapply(apilado$vd,list(apilado[,que.fac[3]]),function(x) mean(x,na.rm=TRUE))
	media.jn=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[3]]),
			function(x) mean(x,na.rm=TRUE))
	media.kn=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,que.fac[3]]),
			function(x) mean(x,na.rm=TRUE))
	media.jkn=NA
	media.lm=tapply(apilado$vd,list(apilado[,que.fac[1]],apilado[,que.fac[2]]),
			function(x) mean(x,na.rm=TRUE))
	media.jlm=NA;	media.klm=NA;	media.jklm=NA
	media.ln=tapply(apilado$vd,list(apilado[,que.fac[1]],apilado[,que.fac[3]]),
			function(x) mean(x,na.rm=TRUE))
	media.jln=NA;	media.kln=NA;	media.jkln=NA
	media.mn=tapply(apilado$vd,list(apilado[,que.fac[2]],apilado[,que.fac[3]]),
			function(x) mean(x,na.rm=TRUE))

	t.medias=list(media.j=media.j,media.k=media.k,media.jk=media.jk,media.l=media.l,
		media.jl=media.jl,media.kl=media.kl,media.m=media.m,media.jm=media.jm,
		media.km=media.km, media.n=media.n,media.jn=media.jn,media.kn=media.kn,
		media.lm=media.lm, media.ln=media.ln,media.mn=media.mn)

	names(t.medias)=c(fac.inter[1],fac.inter[2],
			paste(fac.inter[1],':',fac.inter[2],sep=''),
			que.fac[1],paste(fac.inter[1],':',que.fac[1],sep=''),
			paste(fac.inter[2],':',que.fac[1],sep=''),
			que.fac[2],paste(fac.inter[1],':',que.fac[2],sep=''),
			paste(fac.inter[2],':',que.fac[2],sep=''),
			que.fac[3],paste(fac.inter[1],':',que.fac[3],sep=''),
			paste(fac.inter[2],':',que.fac[3],sep=''),
			paste(que.fac[1],':',que.fac[2],sep=''),
			paste(que.fac[1],':',que.fac[3],sep=''),
			paste(que.fac[2],':',que.fac[3],sep=''))


	n.j=tapply(apilado$vd,list(apilado[,fac.inter[1]]),length)
	n.k=tapply(apilado$vd,list(apilado[,fac.inter[2]]),length)
	n.jk=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,fac.inter[2]]),length)
	n.l=tapply(apilado$vd,list(apilado[,que.fac[1]]),length)
	n.jl=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[1]]),length)
	n.kl=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,que.fac[1]]),length)
	n.jkl=NA
	n.m=tapply(apilado$vd,list(apilado[,que.fac[2]]),length)
	n.jm=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[2]]),length)
	n.km=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,que.fac[2]]),length)
	n.jkm=NA
	n.n=tapply(apilado$vd,list(apilado[,que.fac[3]]),length)
	n.jn=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[3]]),length)
	n.kn=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,que.fac[3]]),length)
	n.jkn=NA
	n.lm=tapply(apilado$vd,list(apilado[,que.fac[1]],apilado[,que.fac[2]]),length)
	n.jlm=NA;	n.klm=NA;	n.jklm=NA
	n.ln=tapply(apilado$vd,list(apilado[,que.fac[1]],apilado[,que.fac[3]]),length)
	n.jln=NA;	n.kln=NA;	n.jkln=NA
	n.mn=tapply(apilado$vd,list(apilado[,que.fac[2]],apilado[,que.fac[3]]),length)

	t.n=list(n.j=n.j,n.k=n.k,n.jk=n.jk,n.l=n.l,
		n.jl=n.jl,n.kl=n.kl,n.m=n.m,n.jm=n.jm,
		n.km=n.km, n.n=n.n,n.jn=n.jn,n.kn=n.kn,
		n.lm=n.lm, n.ln=n.ln,n.mn=n.mn)

	names(t.n)=c(fac.inter[1],fac.inter[2],
			paste(fac.inter[1],':',fac.inter[2],sep=''),
			que.fac[1],paste(fac.inter[1],':',que.fac[1],sep=''),
			paste(fac.inter[2],':',que.fac[1],sep=''),
			que.fac[2],paste(fac.inter[1],':',que.fac[2],sep=''),
			paste(fac.inter[2],':',que.fac[2],sep=''),
			que.fac[3],paste(fac.inter[1],':',que.fac[3],sep=''),
			paste(fac.inter[2],':',que.fac[3],sep=''),
			paste(que.fac[1],':',que.fac[2],sep=''),
			paste(que.fac[1],':',que.fac[3],sep=''),
			paste(que.fac[2],':',que.fac[3],sep=''))
    }
    # FIN B2W3
 
    # B3W2
    if(n.inter==3 & n.intra==2) {
	que.fac=names(fac.intra)
	apilado=apila.los.datos.fnc(datos,fac.intra,fac.inter,col.empieza.mr=col.empieza.mr, silente=TRUE)
	if(!is.na(covariante[1])) {
	  new.dat=data.poscovari.fnc(datos, fac.inter=fac.inter, vd=NA, 
		fac.intra=fac.intra, covariante=covariante,
		col.empieza.mr=col.empieza.mr, apilados=F)
	  new.dat[,'vd']=new.dat$pred
	  apilado=new.dat
	}   	

	media.j=tapply(apilado$vd,list(apilado[,fac.inter[1]]),function(x) mean(x,na.rm=TRUE))
	media.k=tapply(apilado$vd,list(apilado[,fac.inter[2]]),function(x) mean(x,na.rm=TRUE))
	media.l=tapply(apilado$vd,list(apilado[,fac.inter[3]]),function(x) mean(x,na.rm=TRUE))
	media.jk=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,fac.inter[2]]),
			function(x) mean(x,na.rm=TRUE))
	media.jl=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,fac.inter[3]]),
			function(x) mean(x,na.rm=TRUE))
	media.kl=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,fac.inter[3]]),
			function(x) mean(x,na.rm=TRUE))
	media.jkl=NA
	media.m=tapply(apilado$vd,list(apilado[,que.fac[1]]),function(x) mean(x,na.rm=TRUE))
	media.jm=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[1]]),
			function(x) mean(x,na.rm=TRUE))
	media.km=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,que.fac[1]]),
			function(x) mean(x,na.rm=TRUE))
	media.lm=tapply(apilado$vd,list(apilado[,fac.inter[3]],apilado[,que.fac[1]]),
			function(x) mean(x,na.rm=TRUE))
	media.jkm=NA;	media.jlm=NA;	media.klm=NA;	media.jklm=NA
	media.n=tapply(apilado$vd,list(apilado[,que.fac[2]]),function(x) mean(x,na.rm=TRUE))
	media.jn=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[2]]),
			function(x) mean(x,na.rm=TRUE))
	media.kn=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,que.fac[2]]),
			function(x) mean(x,na.rm=TRUE))
	media.ln=tapply(apilado$vd,list(apilado[,fac.inter[3]],apilado[,que.fac[2]]),
			function(x) mean(x,na.rm=TRUE))
	media.jkn=NA; media.jln=NA;	media.kln=NA; media.jkln=NA;
	media.mn=tapply(apilado$vd,list(apilado[,que.fac[1]],apilado[,que.fac[2]]),
			function(x) mean(x,na.rm=TRUE))

	t.medias=list(media.j=media.j,media.k=media.k,media.l=media.l, 
		media.jk=media.jk, media.jl=media.jl, media.kl=media.kl,
		media.m=media.m,media.jm=media.jm,
		media.km=media.km,media.lm=media.lm, media.n=media.n,
		media.jn=media.jn, media.kn=media.kn, media.ln=media.ln,media.mn=media.mn)

	names(t.medias)=c(fac.inter[1],fac.inter[2],fac.inter[3],
			paste(fac.inter[1],':',fac.inter[2],sep=''),
			paste(fac.inter[1],':',fac.inter[3],sep=''),
			paste(fac.inter[2],':',fac.inter[3],sep=''),
			que.fac[1],paste(fac.inter[1],':',que.fac[1],sep=''),
			paste(fac.inter[2],':',que.fac[1],sep=''),
			paste(fac.inter[3],':',que.fac[1],sep=''),
			que.fac[2],paste(fac.inter[1],':',que.fac[2],sep=''),
			paste(fac.inter[2],':',que.fac[2],sep=''),
			paste(fac.inter[3],':',que.fac[2],sep=''),
			paste(que.fac[1],':',que.fac[2],sep=''))

	n.j=tapply(apilado$vd,list(apilado[,fac.inter[1]]),length)
	n.k=tapply(apilado$vd,list(apilado[,fac.inter[2]]),length)
	n.l=tapply(apilado$vd,list(apilado[,fac.inter[3]]),length)
	n.jk=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,fac.inter[2]]),
			length)
	n.jl=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,fac.inter[3]]),
			length)
	n.kl=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,fac.inter[3]]),
			length)
	n.jkl=NA
	n.m=tapply(apilado$vd,list(apilado[,que.fac[1]]),length)
	n.jm=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[1]]),length)
	n.km=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,que.fac[1]]),length)
	n.lm=tapply(apilado$vd,list(apilado[,fac.inter[3]],apilado[,que.fac[1]]),length)
	n.jkm=NA;	n.jlm=NA;	n.klm=NA;	n.jklm=NA
	n.n=tapply(apilado$vd,list(apilado[,que.fac[2]]),length)
	n.jn=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[2]]),length)
	n.kn=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,que.fac[2]]),length)
	n.ln=tapply(apilado$vd,list(apilado[,fac.inter[3]],apilado[,que.fac[2]]),length)
	n.jkn=NA; n.jln=NA;	n.kln=NA; n.jkln=NA;
	n.mn=tapply(apilado$vd,list(apilado[,que.fac[1]],apilado[,que.fac[2]]),length)

	t.n=list(n.j=n.j,n.k=n.k,n.l=n.l, 
		n.jk=n.jk, n.jl=n.jl, n.kl=n.kl,
		n.m=n.m,n.jm=n.jm,
		n.km=n.km,n.lm=n.lm, n.n=n.n,
		n.jn=n.jn, n.kn=n.kn, n.ln=n.ln,n.mn=n.mn)

	names(t.n)=c(fac.inter[1],fac.inter[2],fac.inter[3],
			paste(fac.inter[1],':',fac.inter[2],sep=''),
			paste(fac.inter[1],':',fac.inter[3],sep=''),
			paste(fac.inter[2],':',fac.inter[3],sep=''),
			que.fac[1],paste(fac.inter[1],':',que.fac[1],sep=''),
			paste(fac.inter[2],':',que.fac[1],sep=''),
			paste(fac.inter[3],':',que.fac[1],sep=''),
			que.fac[2],paste(fac.inter[1],':',que.fac[2],sep=''),
			paste(fac.inter[2],':',que.fac[2],sep=''),
			paste(fac.inter[3],':',que.fac[2],sep=''),
			paste(que.fac[1],':',que.fac[2],sep=''))
    }
    # FIN B3W3

    # B3W2
    if(n.inter==3 & n.intra==3) {
	que.fac=names(fac.intra)
	apilado=apila.los.datos.fnc(datos,fac.intra,fac.inter,col.empieza.mr=col.empieza.mr, silente=TRUE)
	if(!is.na(covariante[1])) {
	  new.dat=data.poscovari.fnc(datos, fac.inter=fac.inter, vd=NA, 
		fac.intra=fac.intra, covariante=covariante,
		col.empieza.mr=col.empieza.mr, apilados=F)
	  new.dat[,'vd']=new.dat$pred
	  apilado=new.dat
	}   	

	media.j=tapply(apilado$vd,list(apilado[,fac.inter[1]]),function(x) mean(x,na.rm=TRUE))
	media.k=tapply(apilado$vd,list(apilado[,fac.inter[2]]),function(x) mean(x,na.rm=TRUE))
	media.l=tapply(apilado$vd,list(apilado[,fac.inter[3]]),function(x) mean(x,na.rm=TRUE))
	media.jk=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,fac.inter[2]]),
			function(x) mean(x,na.rm=TRUE))
	media.jl=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,fac.inter[3]]),
			function(x) mean(x,na.rm=TRUE))
	media.kl=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,fac.inter[3]]),
			function(x) mean(x,na.rm=TRUE))
	media.m=tapply(apilado$vd,list(apilado[,que.fac[1]]),function(x) mean(x,na.rm=TRUE))
	media.jm=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[1]]),
			function(x) mean(x,na.rm=TRUE))
	media.km=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,que.fac[1]]),
			function(x) mean(x,na.rm=TRUE))
	media.lm=tapply(apilado$vd,list(apilado[,fac.inter[3]],apilado[,que.fac[1]]),
			function(x) mean(x,na.rm=TRUE))
	media.n=tapply(apilado$vd,list(apilado[,que.fac[2]]),function(x) mean(x,na.rm=TRUE))
	media.jn=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[2]]),
			function(x) mean(x,na.rm=TRUE))
	media.kn=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,que.fac[2]]),
			function(x) mean(x,na.rm=TRUE))
	media.ln=tapply(apilado$vd,list(apilado[,fac.inter[3]],apilado[,que.fac[2]]),
			function(x) mean(x,na.rm=TRUE))
	media.o=tapply(apilado$vd,list(apilado[,que.fac[3]]),function(x) mean(x,na.rm=TRUE))
	media.jo=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[3]]),
			function(x) mean(x,na.rm=TRUE))
	media.ko=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,que.fac[3]]),
			function(x) mean(x,na.rm=TRUE))
	media.lo=tapply(apilado$vd,list(apilado[,fac.inter[3]],apilado[,que.fac[3]]),
			function(x) mean(x,na.rm=TRUE))
	media.mn=tapply(apilado$vd,list(apilado[,que.fac[1]],apilado[,que.fac[2]]),
			function(x) mean(x,na.rm=TRUE))
	media.mo=tapply(apilado$vd,list(apilado[,que.fac[1]],apilado[,que.fac[3]]),
			function(x) mean(x,na.rm=TRUE))
	media.no=tapply(apilado$vd,list(apilado[,que.fac[2]],apilado[,que.fac[3]]),
			function(x) mean(x,na.rm=TRUE))

	t.medias=list(media.j=media.j,media.k=media.k,media.l=media.l, 
		media.jk=media.jk, media.jl=media.jl, media.kl=media.kl,
		media.m=media.m,media.jm=media.jm,
		media.km=media.km,media.lm=media.lm, media.n=media.n,
		media.jn=media.jn, media.kn=media.kn, media.ln=media.ln,
		media.o=media.o, media.jo=media.jo, media.ko=media.ko,media.lo=media.lo,
		media.mn=media.mn,media.mo=media.mo,media.no=media.no)


	names(t.medias)=c(fac.inter[1],fac.inter[2],fac.inter[3],
			paste(fac.inter[1],':',fac.inter[2],sep=''),
			paste(fac.inter[1],':',fac.inter[3],sep=''),
			paste(fac.inter[2],':',fac.inter[3],sep=''),
			que.fac[1],paste(fac.inter[1],':',que.fac[1],sep=''),
			paste(fac.inter[2],':',que.fac[1],sep=''),
			paste(fac.inter[3],':',que.fac[1],sep=''),
			que.fac[2],paste(fac.inter[1],':',que.fac[2],sep=''),
			paste(fac.inter[2],':',que.fac[2],sep=''),
			paste(fac.inter[3],':',que.fac[2],sep=''),
			que.fac[3],paste(fac.inter[1],':',que.fac[3],sep=''),
			paste(fac.inter[2],':',que.fac[3],sep=''),
			paste(fac.inter[3],':',que.fac[3],sep=''),
			paste(que.fac[1],':',que.fac[2],sep=''),
			paste(que.fac[1],':',que.fac[3],sep=''),
			paste(que.fac[2],':',que.fac[3],sep=''))

	n.j=tapply(apilado$vd,list(apilado[,fac.inter[1]]),length)
	n.k=tapply(apilado$vd,list(apilado[,fac.inter[2]]),length)
	n.l=tapply(apilado$vd,list(apilado[,fac.inter[3]]),length)
	n.jk=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,fac.inter[2]]),length)
	n.jl=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,fac.inter[3]]),length)
	n.kl=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,fac.inter[3]]),length)
	n.m=tapply(apilado$vd,list(apilado[,que.fac[1]]),length)
	n.jm=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[1]]),length)
	n.km=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,que.fac[1]]),length)
	n.lm=tapply(apilado$vd,list(apilado[,fac.inter[3]],apilado[,que.fac[1]]),length)
	n.n=tapply(apilado$vd,list(apilado[,que.fac[2]]),length)
	n.jn=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[2]]),length)
	n.kn=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,que.fac[2]]),length)
	n.ln=tapply(apilado$vd,list(apilado[,fac.inter[3]],apilado[,que.fac[2]]),length)
	n.o=tapply(apilado$vd,list(apilado[,que.fac[3]]),length)
	n.jo=tapply(apilado$vd,list(apilado[,fac.inter[1]],apilado[,que.fac[3]]),length)
	n.ko=tapply(apilado$vd,list(apilado[,fac.inter[2]],apilado[,que.fac[3]]),length)
	n.lo=tapply(apilado$vd,list(apilado[,fac.inter[3]],apilado[,que.fac[3]]),length)
	n.mn=tapply(apilado$vd,list(apilado[,que.fac[1]],apilado[,que.fac[2]]),length)
	n.mo=tapply(apilado$vd,list(apilado[,que.fac[1]],apilado[,que.fac[3]]),length)
	n.no=tapply(apilado$vd,list(apilado[,que.fac[2]],apilado[,que.fac[3]]),length)

	t.n=list(n.j=n.j,n.k=n.k,n.l=n.l, 
		n.jk=n.jk, n.jl=n.jl, n.kl=n.kl,
		n.m=n.m,n.jm=n.jm,
		n.km=n.km,n.lm=n.lm, n.n=n.n,
		n.jn=n.jn, n.kn=n.kn, n.ln=n.ln,
		n.o=n.o, n.jo=n.jo, n.ko=n.ko,n.lo=n.lo,
		n.mn=n.mn,n.mo=n.mo,n.no=n.no)
	names(t.n)=c(fac.inter[1],fac.inter[2],fac.inter[3],
			paste(fac.inter[1],':',fac.inter[2],sep=''),
			paste(fac.inter[1],':',fac.inter[3],sep=''),
			paste(fac.inter[2],':',fac.inter[3],sep=''),
			que.fac[1],paste(fac.inter[1],':',que.fac[1],sep=''),
			paste(fac.inter[2],':',que.fac[1],sep=''),
			paste(fac.inter[3],':',que.fac[1],sep=''),
			que.fac[2],paste(fac.inter[1],':',que.fac[2],sep=''),
			paste(fac.inter[2],':',que.fac[2],sep=''),
			paste(fac.inter[3],':',que.fac[2],sep=''),
			que.fac[3],paste(fac.inter[1],':',que.fac[3],sep=''),
			paste(fac.inter[2],':',que.fac[3],sep=''),
			paste(fac.inter[3],':',que.fac[3],sep=''),
			paste(que.fac[1],':',que.fac[2],sep=''),
			paste(que.fac[1],':',que.fac[3],sep=''),
			paste(que.fac[2],':',que.fac[3],sep=''))

    }
    # FIN B3W3   
 return(list(t.medias=t.medias,t.n=t.n))
 }
# -------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
#
# -------------------------------------------------------------------------------
 grafica.ic.fnc=function(tablas,MCintra,gli,lim, ylim=NA, to.pdf=FALSE,size.font=1,
		apaisado=FALSE,nombre, color=TRUE){
	colores=colors( )
	que.col=c(169,152,185,217,235,17,252,380,437,438)
	que.col=c(170,193,205,217,229,241,253,403,402,606)
	que.col=c(170,241,253,193,217,205,403,402,606)
	que.colores=colores[que.col]

	if(!is.na(ylim[1])) ylim.ori=TRUE else ylim.ori=FALSE

	require(gplots, quietly = TRUE)
	medias=tablas[[1]]; n=tablas[[2]]
	n.med=length(medias)
	if(n.med > 6 & !to.pdf) {
		to.pdf=TRUE
		cat('*** Warning. Con mas de 3 factores, las graficas se guardan en pdf ***',fill=TRUE)
	}
	if(length(gli) < n.med) gli=rep(gli,n.med)
 	if(length(MCintra) < n.med) MCintra=rep(MCintra,n.med)
        if(to.pdf){ # SI SALIDA A ARCHIVO PDF
		if(is.na(nombre[1])){
			nombre.file='graficas_anova.pdf'
	  		name.file=nombre.file
		}else{
			nombre.file=paste('graf_anova_',nombre,'.pdf',sep='')
			name.file=nombre.file
		}
		if(apaisado) {
			width=12; height=10; paper='a4r'
		}else{
			width=7; height=7; paper='a4'
		}
          pdf(file= name.file ,width=width,height=height,onefile=TRUE, paper=paper)
	  for(i in 1:n.med){
	    if(!ylim.ori) ylim=NA
	    med_=medias[[i]]; nombre=names(medias)[i]
	    n_=n[[i]]; n_=mean(n_); gl_=gli[i];
	    error=sqrt( (MCintra[i]/n_)*qt(0.975,gl_))
	    ic.inf=med_-error;	ic.sup=med_+error
	    if(length(dim(med_))== 1){
	      n.color=dim(med_)
              # CHECK SI NEGATIVOS
              if(is.na(ylim)[1]){
                 # CHECK SI NEGATIVOS
	         if(min(ic.inf) < 0 & max(ic.sup) <=0) {
		   ylim=c(min(ic.inf)-lim*error,0)
	         }
	         if(min(ic.inf)< 0 & max(ic.sup) > 0) {
		   ylim=c(min(ic.inf)-lim*error,max(ic.sup)+lim*error)
	         }
	         if(min(ic.inf) > 0 & max(ic.sup) > 0) {
		   ylim=c(0,max(ic.sup)+lim*error)
	         }
	      }
	      barplot2(med_,ylim=ylim, xpd=FALSE,
			main=paste('IC 95% ',nombre,sep=''),col='grey',
			xlab=nombre,cex.names=size.font,
			beside = T, plot.ci=TRUE, ci.l=ic.inf, ci.u=ic.sup)
	      box( )
	    }else{
		nombres=strsplit(nombre,':')
		fac1=nombres[[1]][1]; fac2=nombres[[1]][2]
	      n.color=dim(med_)[1]
              if(is.na(ylim)[1]){
                 # CHECK SI NEGATIVOS
	         if(min(ic.inf)< 0 & max(ic.sup) <=0) {
		   ylim=c(min(ic.inf)-lim*error,0)
	         }
	         if(min(ic.inf)< 0 & max(ic.sup) > 0) {
		   ylim=c(min(ic.inf)-lim*error,max(ic.sup)+lim*error)
	         }
	         if(min(ic.inf) > 0 & max(ic.sup) > 0) {
		   ylim=c(0,max(ic.sup)+lim*error)
	         }
	      }
		 if(color) colores=rainbow(n.color) else colores=que.colores[1:n.color]
	      barplot2(med_,ylim=ylim, xpd=FALSE,
	      	main=paste('IC 95% ',nombre,sep=''), col=colores,legend.text=TRUE,
			xlab=fac2,cex.names=size.font,
	      	beside = T, plot.ci=TRUE, ci.l=ic.inf, ci.u=ic.sup)
	     	      n.color=dim(t(med_))[1]
              box()
		n.color=dim(t(med_))[1]
		if(color) colores=rainbow(n.color) else colores=que.colores[1:n.color]
		barplot2(t(med_),ylim=ylim, xpd=FALSE,
		  main=paste('IC 95% ',nombre,sep=''),col=colores,legend.text=TRUE,
		  xlab=fac1, cex.names=size.font,
		  beside = T, plot.ci=TRUE, ci.l=t(ic.inf), ci.u=t(ic.sup))
		box()
	    }
	 }
        dev.off()
        cat('*** Se ha creado el archivo pdf',nombre.file,'en el directorio activo    ***',fill=TRUE)
	  if(nombre.file=='graficas_anova.pdf'){
	    cat('',fill=TRUE)
	    cat('*** Si deseas otro nombre para el archivo incluye el argumento nombre  ***',fill=TRUE)
	    cat("*** Ej: nombre='stress1' y se creara el archivo graf_anova_stress1.pdf ***",fill=TRUE)
	  }
	# CIERRA PDF SI
        }else{
	  # SI SALIDA A SCREEN
	  for(i in 1:n.med){
	    if(!ylim.ori) ylim=NA
	    med_=medias[[i]]; nombre=names(medias)[i]
	    n_=n[[i]]; n_=mean(n_); gl_=gli[i];
	    error=sqrt( (MCintra[i]/n_)*qt(0.975,gl_))
	    ic.inf=med_-error;	ic.sup=med_+error

	    # SI UNIFACTOR
	    if(length(dim(med_))== 1){
	      n.color=dim(med_)
              if(is.na(ylim)[1]){
                 # CHECK SI NEGATIVOS
	         if(min(ic.inf)< 0 & max(ic.sup) <=0) {
		   ylim=c(min(ic.inf)-lim*error,0)
	         }
	         if(min(ic.inf)< 0 & max(ic.sup) > 0) {
		   ylim=c(min(ic.inf)-lim*error,max(ic.sup)+lim*error)
	         }
	         if(min(ic.inf) > 0 & max(ic.sup) > 0) {
		   ylim=c(0,max(ic.sup)+lim*error)
	         }
	      }
	      barplot2(med_,ylim= ylim, xpd=FALSE,
			main=paste('IC 95% ',nombre,sep=''),col='grey',
			xlab=nombre,cex.names=size.font,
			beside = T, plot.ci=TRUE, ci.l=ic.inf, ci.u=ic.sup)
	      box( )
		 if(n.med != 1) X11( )
	    }else{
		nombres=strsplit(nombre,':')
		fac1=nombres[[1]][1]; fac2=nombres[[1]][2]
	      n.color=dim(med_)[1]
              if(is.na(ylim)[1]){
                 # CHECK SI NEGATIVOS
	         if(min(ic.inf)< 0 & max(ic.sup) <=0) {
		   ylim=c(min(ic.inf)-lim*error,0)
	         }
	         if(min(ic.inf)< 0 & max(ic.sup) > 0) {
		   ylim=c(min(ic.inf)-lim*error,max(ic.sup)+lim*error)
	         }
	         if(min(ic.inf) > 0 & max(ic.sup) > 0) {
		   ylim=c(0,max(ic.sup)+lim*error)
	         }
	      }
              if(color) colores=rainbow(n.color) else colores=que.colores[1:n.color]
	      barplot2(med_,ylim=ylim, xpd=FALSE,
	      	main=paste('IC 95% ',nombre,sep=''),col=colores,legend.text=TRUE,
			xlab=fac2,cex.names=size.font,
	      	beside = T, plot.ci=TRUE, ci.l=ic.inf, ci.u=ic.sup)
	      box()
	      X11( )
	      n.color=dim(t(med_))[1]
		if(color) colores=rainbow(n.color) else colores=que.colores[1:n.color]
	      barplot2(t(med_),ylim=ylim, xpd=FALSE,
			main=paste('IC 95% ',nombre,sep=''),col=colores,legend.text=TRUE,
			xlab=fac1,cex.names=size.font,
			beside = T, plot.ci=TRUE, ci.l=t(ic.inf), ci.u=t(ic.sup))
	      box()
		if(i != n.med) X11( )
	    } # FIN SI UNIFACTOR
	 } # FIN SI SALIDA A SCREEN

     } # CIERRA PDF NO
	try(detach(package:gplots),silent=TRUE)
 }
# -------------------------------------------------------------------------------

#---------------------------------------------------------------------------
# regresion.logistica.fnc(dat,grupo,paso.a.paso=T)
#---------------------------------------------------------------------------
regresion.logistica.fnc=function(datos, variables=NA, grupo, paso.a.paso=TRUE, 
                                 grafica=FALSE, guarda.pf=FALSE, latex=FALSE, 
                                 interaccion=FALSE, br=FALSE){
  nombres=names(datos)
  row.names(datos)=1:dim(datos)[1]
  dat=datos
  
  if(!br) crea.cat.fnc('REGRESION LOGISTICA') else 
    crea.cat.fnc('REGRESION LOGISTICA  Maximum Penalized Likelihood')
  
  require(car, quietly=TRUE)
  
  dat[,grupo]=factor(dat[,grupo])
  niveles=levels(dat[,grupo])
  n.group=nlevels(dat[,grupo])
  
  if(length(niveles) !=2){
    cat('',fill=TRUE)
    cat('*** Error. Por el momento la variable criterio debe tener dos niveles ***',fill=TRUE)
    cat('*** Recodifica la variable del argumento grupo apropiadamente         ***',fill=TRUE)
    cat('',fill=TRUE)
    stop()
  }
  col.grupo=match(grupo,nombres)
  if(is.na(col.grupo[1])){
    cat('', fill=TRUE)
    cat('*** Error. No existe la variable:',grupo,'en la base de datos introducida ***', fill=TRUE)
    cat('', fill=TRUE)
    stop( )
  }
  if(is.na(variables[1])){
    variables=nombres[-col.grupo]
    dat=dat[,c(grupo,variables)]
  }else{
    if(is.numeric(variables)) variables=nombres[variables]
    col.grupo=match(grupo,variables)
    if(!is.na(col.grupo[1])) variables=variables[-col.grupo]
    dat=dat[,c(grupo,variables)]
  }
  
  if(interaccion){
    grafica=FALSE
    modelo=paste(grupo,' ~ .*.',sep='')
    paso.a.paso=FALSE
    mod.lg=step(glm(eval(parse(text=paste(grupo,' ~ .*.',sep=''))),
                    family=binomial(link = "logit"),x=TRUE, data=dat))
    selec=names(coef(mod.lg)[-1])
    lista=strsplit(selec,':')
    indice=sapply(lista, function(x) length(x)==1)
    variables=as.character(do.call(cbind,lista[indice]))
    variables=busca.factores.fnc(dat,grupo,variables)
    dat=na.omit(dat[,c(grupo,variables)])
    if(br) modelo=mod.lg$formula
  }else{	
    modelo=paste(grupo,' ~ .',sep='')
  }
  
  if(paso.a.paso){
    require('klaR', quietly = TRUE)
    gw_obj = greedy.wilks(eval(parse(text=modelo)),
                          data = dat, niveau = 0.1)
    selec=as.character(gw_obj$results[,1])
    lista=strsplit(selec,':')
    indice=sapply(lista, function(x) length(x)==1)
    variables=as.character(do.call(cbind,lista[indice]))			
    variables=busca.factores.fnc(dat,grupo,variables)
    dat=na.omit(dat[,c(grupo,variables)])
    mod.lg=glm(eval(parse(text=modelo)),family=binomial(link = "logit"),
               data=dat)
  }
  if(!paso.a.paso & !interaccion & !br){
    mod.lg=glm(eval(parse(text=modelo)),family=binomial,
               data=dat)
  }
  if(!paso.a.paso & !interaccion & br){
    require(brglm, quietly=TRUE)
    mod.lg=brglm(eval(parse(text=modelo)),family=binomial,
                 data=dat)
    try(detach(package:brglm),silent=TRUE)
  }	
  if(!paso.a.paso & interaccion & br){
    require(brglm, quietly=TRUE)
    mod.lg=brglm(modelo,family=binomial,
                 data=dat)
    detach(package:brglm)
  }	
  
  # Cada predictora en relacion al resto
  if(!br)	si.drop=drop1(mod.lg,test='Chi') else si.drop=NA
  dat$p.predicha=mod.lg$fitted.values
  
  if(is.factor(dat[,grupo])) {
    etiquetas=levels(dat[,grupo]) 
  }else{
    etiquetas=unique(dat[,grupo])
  }
  dat=discretiza.variable.fnc(dat,variable='p.predicha',cortes=c(0,0.5,1),
                              var.out='grupo.predicho', etiquetas=etiquetas,silente=TRUE)
  if(grupo=='grupo'){
    tabla=with(dat,table(dat[,'grupo'],grupo.predicho))
  }else{
    tabla=with(dat,table(dat[,grupo],grupo.predicho))
  }
  colnames(tabla)=niveles
  row.names(tabla)=niveles
  nombre.tabla=dimnames(tabla)
  names(nombre.tabla)=c('Observado','Predicho')
  dimnames(tabla)=nombre.tabla
  p.tabla=prop.table(tabla,1)
  gli=mod.lg$df.null-mod.lg$df.residual
  dist=mod.lg$null.deviance-mod.lg$deviance
  p.value=1-pchisq(dist,gli)
  ajuste=round(c(Chi.cuadrado=dist,gl=gli,p=p.value),4)
  coe=round(summary(mod.lg)$coefficients,4)
  coe=data.frame(coe)
  coe$exp.B=round(exp(coe$Estimate),2)
  names(coe)=c('B','ET','z','p','exp(B)')
  variables=data.frame(variables)
  if(!br)	anova.res=Anova(mod.lg) else anova.res=NA
  salida=list(modelo=mod.lg,variables.seleccionadas=variables,ajuste=ajuste,
              coeficientes=coe,cada.predictora=si.drop,
              clasificacion=tabla, prop.clasificacion=p.tabla, Anova=anova.res)
  
  if(interaccion){
    cat('',fill=TRUE)
    cat('*** WARNING. Has solicitado un modelo con interaccion. Es muy importante',fill=TRUE)
    cat('*** que las variables cuantitativas entren centradas. Para ello puedes',fill=TRUE)
    cat('*** utilizar la funcion centra.variable.fnc.',fill=TRUE)
    cat('*** Si quisieras centrar las variables 2,3,4 y 6 puedes hacerlo asi:',fill=TRUE)
    cat('*** Ej: for(i in c(2:4,6)) datos=centra.variables.fnc(datos, variable=i)',fill=TRUE)
    cat('',fill=TRUE)	
    tiempo=Sys.time()
    tiempo=strsplit(as.character(tiempo),' ')
    hora=strsplit(tiempo[[1]][2],':')[[1]]
    hora=paste(hora[1],hora[2],hora[3],sep='_')
    fecha=paste(tiempo[[1]][1],hora,sep='_')		
    require(sjPlot, quietly=TRUE)
    nombre.pdf=paste('graf_interac_reg_log_',fecha,'.pdf',sep='')
    pdf(file=nombre.pdf, width=12, height=10, paper='a4r')
    no.interac=try(sjp.lm.int_(mod.lg,is.log=TRUE, variables=variables, br=br),silent=TRUE)
    dev.off( )
    n.elem=length(salida)
    nomb.tab=names(salida)
    if(no.interac=='no.interaccion'){
      v1='*** Ninguna interaccion ha resultado significativa      ***'
      v2='*** No se genera archivo de salida grafico              ***'
    }
    if(no.interac=='interaccion'){
      v1='*** Se ha creado el archivo graf_interac_reg_log.pdf con las graficas'
      v2='*** de las interacciones que han resultado significativas.'
    } 
    if(no.interac=='solo.variables.cualitativas'){
      v1='*** Todas las interacciones son de variables cualitativas.'
      v2='*** No se creara ninguna grafica de interaccion'
    } 		
    
    etiqu=rbind(v1,v2); row.names(etiqu)=c('',''); colnames(etiqu)=''
    salida[[n.elem+1]]=etiqu
    nomb.tab[n.elem+1]='Graficas.Interaccion'
    names(salida)=nomb.tab		
  }	
  print(salida)
  if(latex) latex.fnc(salida)
  
  # ABRE SI GRAFICA
  if(grafica){
    par(mfrow=c(2,2))
    plot(mod.lg)
    # DETERMINA SI HAY FACTORES ENTRE LAS COVARIANTES
    que.factores=dime.si.hay.factores.fnc(dat)$factores
    if(is.data.frame(variables)) variables=as.character(variables$variables)
    chivato.fac=variables %in% que.factores
    if(sum(chivato.fac)==0) variables_=variables else variables_=variables[!chivato.fac]
    # Si hay factores	
    if(sum(chivato.fac)!=0){
      que.var=match('TRUE',chivato.fac)
      n.niveles=nlevels(dat[,variables[chivato.fac]])
      n.niveles=n.niveles-1
      chivato=list( )
      # Si solo 1 factor
      if(length(que.var)==1){
        for(i in 1:length(chivato.fac)){
          if(i==que.var) {
            chivato[[i]]=rep(chivato.fac[i],n.niveles)
          }else{				
            chivato[[i]]=chivato.fac[i]
          }			
        }
        # Si mas de 1 factor
      }else{
        for(i in 1:length(chivato.fac)){
          for(k in 1:length(que.var)){
            if(i==que.var[k]){
              chivato[[i]]=rep(chivato.fac[i],n.niveles[k])
            }			
          }
        }
      }
      # FIN SI HAY FACTORES 
      # montamos el chivato para ser utilizado en la extraccion de pesos sobre
      # las variables cuantitativas y desechando los pesos de factores		
      chivato=lapply(chivato, function(x){
        if(is.null(x)) x=FALSE else x=x })
      selec=logical( )
      for(j in 1:length(chivato.fac)) selec=c(selec,chivato[[j]])
    }# FIN MONTAJE DEL BULIANO PARA PESOS
    
    pesos=coe[,1]
    b0=pesos[1]
    pesos=pesos[-1]
    if(sum(chivato.fac)!=0) pesos=pesos[!selec]
    
    par(mfrow=c(1,1))
    if(is.data.frame(variables_)) variables_=as.character(variables_$variables)
    x11( )
    if(length(variables_)!=0){
      desc=descriptivos.fnc(dat,variables=variables_, traspuesta=TRUE, silente=TRUE)
      desc=desc[2:3,]
      # PLOT POR VARIABLE CUANTITATIVA
      for(i in 1:length(variables_)){
        if(is.null(dim(desc))) x=rnorm(1000,desc[1],desc[2]) else x=rnorm(1000,desc[1,i],desc[2,i])
        z=b0+(pesos[i]*x)
        p=1/(1+exp(-z)) 
        plot(x,p, xlab=variables_[i], ylab='p', col='red', 
             main='Regresion Parcial')
        unidos=data.frame(as.matrix(cbind(p,x)))
        vertical=mean(unidos[unidos$p >=0.5 & unidos$p <= 0.52,]$x)
        abline(h=0.5, lty=2)
        abline(v=vertical, lty=2)
        #lines(dat[,variables[1]], predict(eso,type='response'), type='p')
        if(i != length(variables_) ) x11( )
      } # FIN PLOT POR VARIABLE
    } # FIN SI VARIABLES_ !=0 
  } # CIERRA SI GRAFICA
  try(detach(package:klaR),silent=TRUE)
  if(guarda.pf) {
    datos$index=as.numeric(row.names(datos))
    dat$index=as.numeric(row.names(dat))
    datos=merge(datos, dat[,c('index','p.predicha','grupo.predicho')],by='index')
    datos=datos[,-1]
    return(datos)
  }else{
    return(salida)
  }
}
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# busca.factores.fnc(dat,grupo,variables.seleccionadas.en.greedy.wilks)
#---------------------------------------------------------------------------
 busca.factores.fnc=function(datos,grupo,variables){
	dat=datos
	# Determina que covariante es factor.
	nombres=names(dat)
	pos.gr=match(grupo,nombres)
	check=data.frame(dat[,-pos.gr])
	nombres=nombres[-pos.gr]
	names(check)=nombres
	# Busca factores
	es.factor=logical( )
	for (i in 1:dim(check)[2])
		 es.factor[i]=is.factor(check[,i])

	# SI HAY FACTORES EN LAS VARIABLES DE ENTRADA
	if(sum(es.factor) > 0){
		niveles=list( )
		factores=nombres[es.factor]
		check=data.frame(check[,es.factor])
		if(dim(check)[2]==1) names(check)=factores
		for(i in 1:sum(es.factor)){
			niveles[[i]]=levels(check[,i])
			niveles[[i]]=paste(factores[i],niveles[[i]],sep='')
		} # Cierra busca factores
		names(niveles)=factores

		# Busca concordancias con las variables seleccionadas en el paso a paso
		indice=variables %in% nombres
		new.var=variables[indice]
		son.factores=variables[!indice]
		selec=logical()
		for(i in 1:length(niveles)){
			indice=son.factores %in% niveles[[i]]
			res=son.factores[indice]
			if(length(res)!=0) selec[i]=TRUE else selec[i]=FALSE
		}
		que.fac=factores[selec]
		new.var=c(new.var,que.fac)
		variables=new.var
	} # EL BUBLE SIGUE CON EL NUEVO VECTOR DE VARIABLES DEFINIDO
 return(variables)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#
#---------------------------------------------------------------------------
 recodifica.a.dummy.fnc=function(datos,variable){
	dat=datos
    niveles=length(unique(as.character(dat[,variable])))
    if(niveles > 2){
      cat('*** Tu variable ',variable,'tiene ',niveles,' niveles            ***',fill=TRUE)
      cat('*** El numero maximo de niveles para transformar a dummy ha de ser 2 ***',fill=TRUE)
      cat('*** Puedes recodificar previamente la variable a solo 2 niveles y    ***',fill=TRUE)
      cat('*** posteriormente llamar nuevamente a esta funcion.                 ***',fill=TRUE)
    }
 dat[,variable]=as.numeric(as.factor(dat[,variable]))-1
 tabla=table(dat[,variable])
 lista=list(tabla=tabla)
 names(lista)=variable
 cat('*** Se ha recodificado la variable ',variable,' a su version dummy (0,1) ***',fill=TRUE)
 print(lista)
 return(dat)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# modelo.jerarquico.fnc(alumnos,vd='MathAch',fac.random='cses',random='School')
#---------------------------------------------------------------------------
 modelo.jerarquico.fnc=function(datos,vd,variables=NA,fac.random=NA,random,grafica=FALSE,
      cualitativa=NA, silente=FALSE, latex=FALSE){
	dat=datos
    crea.cat.fnc('MODELO JERARQUICO')
	if(latex){
		cat('',fill=TRUE)
		cat('*** modelo.jerarquico.fnc no admite por el momento el argumento latex ***',fill=TRUE)
		cat('',fill=TRUE)
	}	    
    if(is.na(variables[1])){
      cat('',fill=TRUE) 
      cat('*** Error. No has definido ninguna variable predictora en el argumento variables  ***',fill=TRUE)
      cat('*** Incluye el numero, numeros o nombres de al menos una variable predictora.     ***',fill=TRUE)
      cat("*** Ex. variables='SES', variables=2, variables=c('MEANSES*c.SES','Sector*c.SES') ***",fill=TRUE)
      cat('',fill=TRUE)    
      stop( )
    }
    if(!is.na(cualitativa) & is.na(fac.random)){
      cat('',fill=TRUE)
      cat('*** Error. Deseas ver un grafico de intercepto vs pendiente, parcializado por cada   ***',fill=TRUE)
      cat('*** nivel de una variable cualitativa, pero no has declarado un factor con pendiente ***',fill=TRUE)
      cat('*** random. Incluye en el argumento fac.random el nombre del factor que presupones   ***',fill=TRUE)
      cat("*** aleatorio. Ej: fac.random='SES'                                                  ***",fill=TRUE)
      cat('',fill=TRUE)     
      stop()
    }
      
    nombres=names(dat)
    require(lmerTest, quietly = TRUE) ; require(car, quietly = TRUE)
    if(is.numeric(variables)) variables=nombres[variables]
    n.var=length(variables)
    if(n.var==1) {
      acumula=variables
    }else{
      acumula=variables[1]
      for(i in 2:length(variables))
	acumula=paste(acumula,'+',variables[i],sep='')
    }
    modelo.0=paste(vd,' ~ 1 + ( 1 |',random,')',sep='')
    mod.0=lmer(eval(parse(text=modelo.0)),data=dat)
    #crea.cat.fnc('Modelo.Incondicional')
    #print(summary(mod.0))
    ic=calcula.intraclass.fnc(mod.0,random,random.item=FALSE)
    names(ic)=paste('cor.intraclass.',random,sep='')
	try(assign('dat',dat,envir=.GlobalEnv),silent=TRUE) 
	
    if(is.na(fac.random[1])) {
		modelo.1=paste(vd,' ~ ',acumula,'+ ( 1 |',random,')',sep='')
		mod.1=lmer(eval(parse(text=modelo.1)),data=dat)
		print(qqPlot(ranef(mod.1)[[random]],ylab='Random(Bo)',
			main=' qqplot: Bo'))
		anova1=anova(mod.1)

		# NUMERO DE PARAMETROS ESTIMADOS
		mod= list(gl=n.var +1 + 2,deviance=deviance(mod.1)[1])

		lista=list(modelo.incondicional=mod.0 ,mod=mod,
			modelo=modelo.1,ic=ic,resultado=summary(mod.1),anova.mod=anova1)
    }else{
		modelo.1=paste(vd,' ~ ',acumula,'+ ( 1 |',random,')',sep='')
		modelo.2=paste(vd,' ~ ',acumula,'+ (',fac.random,'|',random,')',sep='')
		mod.1=lmer(eval(parse(text=modelo.1)),data=dat)
		nombres=names(ranef(mod.1)[[1]])
		nombres[1]='Bo'
		mod.2=lmer(eval(parse(text=modelo.2)),data=dat)

	# NUMERO DE PARAMETROS ESTIMADOS
		nvar=length(variables)
		mod= list(gl=nvar+1 + 3,deviance=deviance(mod.2)[1])
		c.anova=anova(mod.1,mod.2)
		par(mfrow=c(2,2))
		print(qqPlot(ranef(mod.1)[[random]],ylab='Random(Bo)',
			main='Modelo.1, qqplot: Bo'))
		plot(0,0,type='n',yaxt='n', xaxt='n',xlab='',ylab='',frame.plot=FALSE)
		print(qqPlot(ranef(mod.2)[[random]][1],ylab='Random(Bo)',
			main='Modelo.2 qqplot: Bo'))
		print(qqPlot(ranef(mod.2)[[random]][2],
			ylab=paste('Random(B.',nombres[2],')',sep=''),
			main=paste('Modelo.2 qq.plot: B.',nombres[2],sep='')))
		par(mfrow=c(1,1))
        anova2=anova(mod.2)
		lista=list(modelo.incondicional=mod.0 ,mod=mod,modelo=modelo.2,
			alternativo=modelo.1,ic=ic, resultado=summary(mod.2),
			anova=c.anova,anova.mod=anova2)

	# SI SE SOLICITA GRAFICA Bo x Pendiente
		if(grafica){
			ran=ranef(mod.2)[[random]]
			names(ran)=c('Bo',paste('B.',fac.random,sep=''))
			nombres.ran=names(ran)
			ran[,random]=row.names(ran)
			dat=merge(dat,ran, by=random)
			ylim=c(min(ran$Bo),max(ran$Bo))
			xlim=c(min(ran[,2]),max(ran[,2]))
			# Si se pide grafico
			# Si no cualitativa
			if(is.na(cualitativa)) {
				X11( )
				plot(ran[,nombres.ran[2]],ran[,nombres.ran[1]],pch=19,
					col='blue', cex=1, xlab=nombres.ran[2],
					ylab=nombres.ran[1],ylim=ylim,xlim=xlim)
					abline(v=0,lty=2)
					abline(h=0,lty=2)
			}else{ # Si cualitativa
	    		x.cualitativa=divide.por.factor.fnc(dat,que.factor=cualitativa, silente=TRUE)
	    		n.lev=length(x.cualitativa)
	    		nombres=names(x.cualitativa)
	    		inici=character()
	    		for(i in nombres) inici[i]=substr(i,1,1)
	    		for(i in 1:n.lev){
	      		x=x.cualitativa[[i]]
	      		if(i==1) {
					X11( )
					plot(x[,nombres.ran[2]],x[,nombres.ran[1]],
						pch=as.character(inici[i]),
						main=paste('Bo.vs.',nombres.ran[2],' - ',cualitativa,sep=''),
						xlab=nombres.ran[2],ylab=nombres.ran[1],
						col=i, cex=0.6, ylim=ylim,xlim=xlim)
	      		}else{
					points(x[,nombres.ran[2]],x[,nombres.ran[1]],
						pch=as.character(inici[i]),
						col=i,cex=0.6)
	      		}
	    		# Cierre
	    		abline(v=0,lty=2)
	    		abline(h=0,lty=2)
			} # Fin de si cualitativa
    	}
   	} # FIN DE SI GRAFICA
    } # Fin de si random
	try(detach(package:lme4),silent=TRUE)
	#try(detach(package:car),silent=TRUE)
 if(silente) return(lista) else print(lista)   
# if(latex) latex.fnc(lista)
 }
# vd = 'MathAch'
# variables=c(2,3,4,6,7,8)
# variables='cses'
# variables=c('cses','Sector')
# variables=c('MEANSES*cses','Sector*cses')
# fac.random='cses'
# random='School'
# cualitativa='Sector'
# cualitativa='Sex'
# cualitativa= 'Minority'
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# compara.modelos.fnc=function(modelo1,modelo2)
# Compara el parametro deviance de ambos modelos mer y a partir de la
# diferencia lleva a cabo un contraste de ji cuadrado
#---------------------------------------------------------------------------
 compara.modelos.fnc=function(mod1,mod2){
	if(class(mod1$resultado)!='summary.merMod'){
		cat('',fill=TRUE)
		cat('Error. Esta funcion solo compara modelos alternativos jerarquicos ***',fill=TRUE)
		cat('obtenidos a partir de la funcion modelos.jerarquicos.fnc          ***',fill=TRUE)
		stop( )
	}
	crea.cat.fnc('Comparacion de modelos jerarquicos alternativos')
	modelo1=mod1$mod;	modelo2=mod2$mod
	chi=modelo1$deviance-modelo2$deviance
	dif.gl=abs(modelo1$gl-modelo2$gl)
	p=1-pchisq(abs(chi),dif.gl)

 	tabla.modelo=data.frame(modelo=rbind(mod1$modelo,mod2$modelo),
		deviance=c(modelo1$deviance,modelo2$deviance),
		gl=c(modelo1$gl,modelo2$gl))
 	row.names(tabla.modelo)=c('modelo.1','modelo.2')
 	contraste=data.frame(ChiC=chi, gl=dif.gl, p.val= round(p,5))
 	lista=list(modelos.a.comparar=tabla.modelo,contraste=contraste)
 return(lista)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# grafica.panel.fnc(samp1,vd='MathAch',vi='SES',x.panel='School',titulo='Publicos')
#---------------------------------------------------------------------------
 grafica.panel.fnc=function(datos, vd=NA, que.factor, x.panel=NA, titulo=NA,
			to.pdf=FALSE, regresion=FALSE, orden=NULL, ylim=NA){
    require(lattice, quietly = TRUE)
    if(is.na(x.panel[1])){
		datos$my.fac=as.character('')
		x.panel='my.fac'
     		cat('',fill=TRUE)
     		cat('*** No has utilizado el argumento x.panel. Si deseas una grafica por  ***',fill=TRUE)
     		cat("*** cada nivel o valor de un determinado factor incluye el argumento. ***",fill=TRUE)
     		cat(" Ej. x.panel='sujeto'                                                 ***",fill=TRUE)
     		cat('',fill=TRUE)
    }
	
    nombres=names(datos)
	check.que.factor(que.factor)

	if(is.numeric(x.panel)){
		nombres=names(datos)
		x.panel=nombres[x.panel]
	}

    if(length(que.factor)!=1){
 	acumula=que.factor[1]
	for (i in 2:length(que.factor)) acumula=paste(acumula,que.factor[i],sep=':',collapse=':')
	que.factor=acumula
    }

    que.fac=strsplit(que.factor,':')
    n.fac=length(que.fac[[1]])
    los.factores=que.fac[[1]]
    if(n.fac==2) factores=que.fac[[1]]
    for(i in los.factores) 
      if(is.character(datos[,i])) datos[,i]=as.factor(datos[,i])

	# EXISTEN LAS VARIABLES
	check=existe.variable.fnc(datos,c(los.factores,x.panel))
	if(check$cc !=0) {
		cat('',fill=TRUE)
		cat('*** Error. No existe el factor o factores:',check$faltan,fill=TRUE)
		cat('*** en la base de datos incluida. Revisa los argumentos',fill=TRUE)
		stop( )
	}

    if(!is.na(vd[1])){
		if(is.numeric(vd)) vd=nombres[vd]
		# EXISTEN LAS VARIABLES
		check=existe.variable.fnc(datos,vd)
		if(check$cc !=0) {
			cat('',fill=TRUE)
			cat('*** Error. No existe la variable o variables:',check$faltan,fill=TRUE)
			cat('*** en la base de datos incluida. Revisa el argumento vd',fill=TRUE)
			stop( )
		}

		n.col=match(vd,nombres)
		nombres[n.col]='vd'
		names(datos)=nombres
    }
	exist.var.out=FALSE
	if(!is.integer(datos[,x.panel]) & is.numeric(datos[,x.panel])){
		size=dim(datos)[2]
		var.out=paste(x.panel,'.Q',sep='')
		datos=discretiza.variable.fnc(datos, variable=x.panel, 
			var.out=var.out,ntiles=4, silente=T)
			x.panel=names(datos)[size+1]
	}

    if(is.na(vd[1])) vd='vd'
    if(!to.pdf) X11( )
    
    if(x.panel=='my.fac') datos=na.omit(datos[,c('vd',los.factores,'my.fac')])
    if(x.panel!='my.fac' & !exist.var.out) datos=na.omit(datos[,c('vd',los.factores,x.panel)])
    if(x.panel!='my.fac' & exist.var.out) datos=na.omit(datos[,c('vd',los.factores,var.out)])    
    
#--------------------------------------------------------------------------------------------
  if(n.fac==1){
	 if(is.na(titulo[1])){
      	titulo=paste(vd,' en ',que.factor,':',x.panel,sep='')

	 }else{
      	titulo=paste(titulo,'.  ',vd,' en ',que.factor,':',x.panel,sep='')
	 }
	if(!regresion){
	  if(is.na(ylim[1])){
	      print(xyplot(datos$vd ~ datos[,que.factor] | datos[,x.panel], data=datos, main=titulo,layout=orden,
		  xlab=que.factor, ylab=vd, type='a',lwd=3) )
	  }else{
	      print(xyplot(datos$vd ~ datos[,que.factor] | datos[,x.panel], data=datos, main=titulo,layout=orden,
		  xlab=que.factor, ylab=vd, type='a',lwd=3,ylim=ylim) )
	  }
	}else{
	  if(is.na(ylim[1])){
	      print(xyplot(datos$vd ~ datos[,que.factor] | datos[,x.panel], data=datos, main=titulo,layout=orden,
		  xlab=que.factor, ylab=vd,
		  panel=function(x, y){
		  panel.xyplot(x, y)
		  #panel.loess(x, y, span=1)
		  panel.lmline(x, y, lty=2)
		  }) )
	  }else{
	      print(xyplot(datos$vd ~ datos[,que.factor] | datos[,x.panel], data=datos, main=titulo,layout=orden,
		  xlab=que.factor, ylab=vd, ylim=ylim,
		  panel=function(x, y){
		  panel.xyplot(x, y)
		  #panel.loess(x, y, span=1)
		  panel.lmline(x, y, lty=2)
		  }) )
	  }
	} # FIN REGRESION
  } # FIN SI UN SOLO FACTOR
#--------------------------------------------------------------------------------------------
  if(n.fac==2){
	 if(is.na(titulo[1])){
      	titulo=paste(vd,' en ',factores[1],':',factores[2],sep='')
	 }else{
      	titulo=paste(titulo,'.  ',vd,' en ',factores[1],':',factores[2],sep='')
	 }
	if(!regresion){
	  if(is.na(ylim[1])){
	    xyplot(datos$vd ~ datos[,factores[1]] | datos[,x.panel], groups=datos[,factores[2]],
		main=titulo,layout=orden,
     		xlab=que.factor, ylab=vd, type = c('g','a') ,lwd=3,
		key = simpleKey(text = levels(datos[,factores[2]]),
		space = "right", points = TRUE),
		data=datos)
	  }else{
	    xyplot(datos$vd ~ datos[,factores[1]] | datos[,x.panel], groups=datos[,factores[2]],
		main=titulo,layout=orden,
     		xlab=que.factor, ylab=vd, type = c('g','a') ,lwd=3, ylim=ylim,
		key = simpleKey(text = levels(datos[,factores[2]]),
		space = "right", points = TRUE),
		data=datos)		
	}
	}else{
	  if(is.na(ylim[1])){
	    xyplot(datos$vd ~ datos[,factores[1]] | datos[,x.panel], groups=datos[,factores[2]],
		main=titulo, data=datos, layout=orden, xlab=que.factor,
		key = simpleKey(text = levels(datos[,factores[2]]),space = "right"),
		panel=function(x, y,...){
		panel.xyplot(x, y,...)
		panel.lmline(x, y, lty=2)})	
	  }else{
	    xyplot(datos$vd ~ datos[,factores[1]] | datos[,x.panel], groups=datos[,factores[2]],
		main=titulo, data=datos, layout=orden, ylim=ylim,xlab=que.factor,
		key = simpleKey(text = levels(datos[,factores[2]]),space = "right"),
		panel=function(x, y,...){
		panel.xyplot(x, y,...)
		panel.lmline(x, y, lty=2)})		
	  }
	}
  } # CIERRE N.FAC=2
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# grafica.xy.fnc(dat,c('CP1','CP2',que.factor='sexo',titulo=NA)
#---------------------------------------------------------------------------
 grafica.xy.fnc=function(datos,variables,que.factor=NA,titulo=NA){
	dat=datos

	if(is.na(que.factor[1])){
	  cat('*** Error. Debes indicar un factor en el argumento que.factor  ***',fill=TRUE)
	  cat("*** Ej. que.factor='zona'                                      ***",fill=TRUE)
	  stop( )
	}
	nombres=names(dat)
	if(is.na(match(que.factor,nombres))){
	  cat('*** Error. Has indicado el factor',que.factor,'el cual no parece existir en tu base de datos ***',fill=TRUE)
	  stop( )
	}

	x.factor=split(dat,dat[,que.factor])
	inici=character(); nombres=names(x.factor)
	for(i in nombres) inici[i]=substr(i,1,1)
	centros=mean(dat[,variables])
	if(is.na(titulo[1]))
	titulo=paste(variables[1],variables[2],sep=':')
	titulo=paste(titulo,' en niveles de ',que.factor,sep='')
	plot(dat[,variables[1]],dat[,variables[2]],type='n',
		xlab=variables[1],ylab=variables[2],main=titulo)
	for(i in 1:length(x.factor))
 	with(x.factor[[i]],points(eval(parse(text=variables[1])),
		eval(parse(text=variables[2])),pch=inici[i],col=i))
	abline(v=centros[1],h=centros[2],lty=3)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#
#---------------------------------------------------------------------------
 desapila.los.datos.fnc=function(datos.apilados,fac.intra=NA,fac.inter=NA,por.item=NA,
	silente=FALSE){
   dat_=datos.apilados
   suj=match('sujeto',names(dat_))
   ite=match('item',names(dat_))
   if(!is.na(suj)) {hay.suj=TRUE; hay.item=FALSE} 
   if(!is.na(ite)) {hay.item=TRUE; hay.suj=FALSE}	
   
   # CHECK SI EXISTE LA VARIABLE SUJETO O ITEM EN LA BASE A DESAPILAR
   if(is.na(suj[1]) & is.na(ite[1])){
	cat('',fill=TRUE)
	cat('*** Error. La base de datos a desapilar, deber contener obligatoriamente    ***',fill=TRUE)
	cat('*** la variable sujeto o la variable item. Renombra apropiadamente la       ***',fill=TRUE)
	cat('*** variable que actue como unidad de registro (sujeto o item) al nombre    ***',fill=TRUE)
	cat('*** obligatorio de sujeto o item.                                           ***',fill=TRUE)
	cat("*** Ej. datos=cambia.nombre.var.fnc(datos, antiguo='Codigo',nuevo='sujeto') ***",fill=TRUE)
	cat("*** Ej. datos=cambia.nombre.var.fnc(datos, 'palabra','item')                ***",fill=TRUE)
	cat('',fill=TRUE)
	stop( )
    }
    # FIN CHECK
    
	check.fac.intra.fnc(fac.intra)
	check.factores.fnc(fac.inter,fac.intra)

	# CHECK SI EXISTEN LOS FACTORES INTER
      if(!is.na(fac.inter[1])) {
		n.inter=length(fac.inter)
		ok.fac.inter=check.fac.inter(datos.apilados, fac.inter)
		if(length(ok.fac.inter)!=0)
			stop('*** No existe algun o algunos de los factores inter incluidos ***')
	 }else{
		n.inter=0
	 }
	# FIN CHECK SI EXISTEN LOS FACTORES INTER

   if(!is.na(fac.intra[1])) n.intra=length(names(fac.intra)) else n.intra=0
   if(n.intra==1) formula=paste('vd ~ ',names(fac.intra)[1],sep='')
   if(n.intra==2) formula=paste('vd ~ ',names(fac.intra)[1],':',names(fac.intra)[2],sep='')
   if(n.intra==3) formula=paste('vd ~ ',names(fac.intra)[1],':',
		names(fac.intra)[2],':',names(fac.intra)[3],sep='')
   if(n.intra==4) formula=paste('vd ~ ',names(fac.intra)[1],':',
		names(fac.intra)[2],':',names(fac.intra)[3],':',names(fac.intra)[4],sep='')				
		
	if(n.intra!=0){
	  for(i in 1:length(fac.intra))
	    dat_=reordena.factor.fnc(dat_,que.factor=names(fac.intra)[i],
	      niveles=fac.intra[[i]],silente=TRUE) 
	}		

	# SI USTACK TERMINA EN LISTA
   	dat.us=unstack(dat_,form=formula)
	error=FALSE
   	if(!is.data.frame(dat.us)){
		error=TRUE
		if(hay.suj){
		  cat('',fill=TRUE)
		  cat('*** WARNING *** Todos los sujetos no tienen el mismo numero de respuestas ***',fill=TRUE)
		  cat('*** en las condiciones de medidas repetidas. Esto dara lugar a que se     ***',fill=TRUE)
		  cat('*** eliminen los sujetos con datos incompletos en las medidas repetidas.  ***',fill=TRUE)
		  cat('*** Este es el numero de sujetos resultante al eliminar los incompletos.  ***',fill=TRUE)
		}
		if(hay.item){
		  cat('',fill=TRUE)
		  cat('*** WARNING *** Todos los item no tienen el mismo numero de respuestas ***',fill=TRUE)
		  cat('*** en las condiciones de medidas repetidas. Esto dara lugar a que se  ***',fill=TRUE)
		  cat('*** eliminen los items con datos incompletos en las medidas repetidas. ***',fill=TRUE)
		  cat('*** Este es el numero de items resultante al eliminar los incompletos.  ***',fill=TRUE)
		}
		if(hay.suj){ 
			x.sujeto=split(dat_, dat_$sujeto)
			n.before=length(x.sujeto)
			n.condi.intra=1
			for(k in 1:n.intra) n.condi.intra=n.condi.intra*length(fac.intra[[k]])
			index.s=sapply(x.sujeto, function(x) dim(x)[1]==n.condi.intra)
			x.sujeto=data.frame(do.call(rbind,x.sujeto[index.s]))
			n.after=length(unique(x.sujeto$sujeto))
			if(n.after==0){
				cat('',fill=TRUE)
				cat('*** Error. Una vez agregados los datos. Todos los sujetos carecen de  ***',fill=TRUE)
				cat('*** al menos un valor en al menos una condicion de medidas repetidas. ***',fill=TRUE)
				cat('***                         REVISA TUS DATOS                          ***',fill=TRUE)
				cat('',fill=TRUE)
				stop()
			}	
			dat.us=unstack(x.sujeto,form=formula)			
			if(error){
			    cat('*** N SUJETOS:',dim(dat.us)[1],fill=TRUE)
			    cat('*** Se han eliminado',(n.before-n.after),'sujetos',fill=TRUE)
			}
		}
		if(hay.item){ 
			x.item=split(dat_, dat_$item)
			n.before=length(x.item)
			n.condi.intra=1
			for(k in 1:n.intra) n.condi.intra=n.condi.intra*length(fac.intra[[k]])
			index.i=sapply(x.item, function(x) dim(x)[1]==n.condi.intra)
			x.item=data.frame(do.call(rbind,x.item[index.i]))
			n.after=length(unique(x.item$item))
			if(n.after==0){
				cat('',fill=TRUE)
				cat('*** Error. Una vez agregados los datos. Todos los items carecen de    ***',fill=TRUE)
				cat('*** al menos un valor en al menos una condicion de medidas repetidas. ***',fill=TRUE)
				cat('***                         REVISA TUS DATOS                          ***',fill=TRUE)
				cat('',fill=TRUE)
				stop()
			}				
			dat.us=unstack(x.item,form=formula)			
			if(error) {
			    cat('*** N ITEMS:',dim(dat.us)[1],fill=TRUE)
			    cat('*** Se han eliminado',(n.before-n.after),'items',fill=TRUE)
			}
		}
	}
	# FIN SI USTACK TERMINA EN LISTA

	# CHECK SI HAY SUJETOS O ITEMS
	nombres=names(dat_)
	hay.suj=match('sujeto', nombres)
	hay.item=match('item',nombres)
	# SI HAY SUJETOS
	if(!is.na(hay.suj)){
   		if(n.inter > 0 & n.inter <=3){
      		x.sujeto=split(dat_,dat_$sujeto)
      		if(error) x.sujeto=x.sujeto[index.s]
      		v.inter1=data.frame(
			do.call(rbind,lapply(x.sujeto, function(x)
	    		unique(as.character(x[,fac.inter[1]])))))
      		names(v.inter1)=fac.inter[1]
      		dat.us[,fac.inter[1]]=v.inter1

			# CHECK SI EL FACTOR ES REALMENTE FACTOR
			chivato=is.factor(dat_[,fac.inter[1]])
			if(!chivato){
				cat('',fill=TRUE)
				cat('*** Error. El factor',fac.inter[1],'no pertenece a la clase factor. Modifica la clase',fill=TRUE)
				cat('*** mediante el uso de la funcion transforma.variable.fnc. Sigue el ejemplo: ',fill=TRUE)
				cat("*** datos=transforma.variable.fnc(datos, variable='mifactor', nuevo.tipo='factor')",fill=TRUE)
				cat('',fill=TRUE)
				stop( )
			}
			# FIN CHECK DE FACTOR

			dat.us=reordena.factor.fnc(dat.us,que.factor=fac.inter[1],
				niveles=levels(dat_[,fac.inter[1]]),silente=TRUE)
      		head(dat.us)
   		}
   		if(n.inter == 2){
      		v.inter2=data.frame(
			do.call(rbind,lapply(x.sujeto, function(x)
	    		unique(as.character(x[,fac.inter[2]])))))
      		names(v.inter2)=fac.inter[2]
      		dat.us[,fac.inter[2]]=v.inter2

			# CHECK SI EL FACTOR ES REALMENTE FACTOR
			chivato=is.factor(dat_[,fac.inter[2]])
			if(!chivato){
				cat('',fill=TRUE)
				cat('*** Error. El factor',fac.inter[3],'no pertenece a la clase factor. Modifica la clase',fill=TRUE)
				cat('*** mediante el uso de la funcion transforma.variable.fnc. Sigue el ejemplo: ',fill=TRUE)
				cat("*** datos=transforma.variable.fnc(datos, variable='mifactor', nuevo.tipo='factor')",fill=TRUE)
				cat('',fill=TRUE)
				stop( )
			}
			# FIN CHECK DE FACTOR

			dat.us=reordena.factor.fnc(dat.us,que.factor=fac.inter[2],
				niveles=levels(dat_[,fac.inter[2]]),silente=TRUE)
      		head(dat.us)
   		}
   		if(n.inter == 3){
      		v.inter2=data.frame(
			do.call(rbind,lapply(x.sujeto, function(x)
	    		unique(as.character(x[,fac.inter[2]])))))
      		names(v.inter2)=fac.inter[2]
      		dat.us[,fac.inter[2]]=v.inter2

			# CHECK SI EL FACTOR ES REALMENTE FACTOR
			chivato=is.factor(dat_[,fac.inter[2]])
			if(!chivato){
				cat('',fill=TRUE)
				cat('*** Error. El factor',fac.inter[3],'no pertenece a la clase factor. Modifica la clase',fill=TRUE)
				cat('*** mediante el uso de la funcion transforma.variable.fnc. Sigue el ejemplo: ',fill=TRUE)
				cat("*** datos=transforma.variable.fnc(datos, variable='mifactor', nuevo.tipo='factor')",fill=TRUE)
				cat('',fill=TRUE)
				stop( )
			}
			# FIN CHECK DE FACTOR

			dat.us=reordena.factor.fnc(dat.us,que.factor=fac.inter[2],
				niveles=levels(dat_[,fac.inter[2]]),silente=TRUE)
      		head(dat.us)
      		v.inter3=data.frame(
			do.call(rbind,lapply(x.sujeto, function(x)
	    		unique(as.character(x[,fac.inter[3]])))))
      		names(v.inter3)=fac.inter[3]
      		dat.us[,fac.inter[3]]=v.inter3

			# CHECK SI EL FACTOR ES REALMENTE FACTOR
			chivato=is.factor(dat_[,fac.inter[3]])
			if(!chivato){
				cat('',fill=TRUE)
				cat('*** Error. El factor',fac.inter[3],'no pertenece a la clase factor. Modifica la clase',fill=TRUE)
				cat('*** mediante el uso de la funcion transforma.variable.fnc. Sigue el ejemplo: ',fill=TRUE)
				cat("*** datos=transforma.variable.fnc(datos, variable='mifactor', nuevo.tipo='factor')",fill=TRUE)
				cat('',fill=TRUE)
				stop( )
			}
			# FIN CHECK DE FACTOR
			dat.us=reordena.factor.fnc(dat.us,que.factor=fac.inter[3],
				niveles=levels(dat_[,fac.inter[3]]),silente=TRUE)
      		head(dat.us)
   		}
	}

	# FIN SI HAY SUJETOS

	# SI HAY ITEMS
	if(!is.na(hay.item) & names(fac.intra)[1]!='item'){
   		if(n.inter > 0 & n.inter <=3){
      		x.item=split(dat_,dat_$item)
      		if(error) x.item=x.item[index.i]
			v.inter1=data.frame(do.call(rbind,lapply(x.item, function(x)
					unique(as.character(x[,fac.inter[1]])))))
			names(v.inter1)=fac.inter[1]
			dat.us[,fac.inter[1]]=v.inter1
   		}
   		if(n.inter==2){
			v.inter2=data.frame(do.call(rbind,lapply(x.item, function(x)
					unique(as.character(x[,fac.inter[2]])))))
			names(v.inter2)=fac.inter[1]
      		dat.us[,fac.inter[2]]=v.inter2
      		head(dat.us)
   		}
   		if(n.inter==3){
      		v.inter2=data.frame(
			do.call(rbind,lapply(x.item, function(x)
	    		unique(as.character(x[,fac.inter[2]])))))
      		names(v.inter2)=fac.inter[2]
      		dat.us[,fac.inter[2]]=v.inter2
      		head(dat.us)
	     		v.inter3=data.frame(
			do.call(rbind,lapply(x.item, function(x)
	    		unique(as.character(x[,fac.inter[3]])))))

      		names(v.inter3)=fac.inter[3]
      		dat.us[,fac.inter[3]]=v.inter3
      		head(dat.us)
   		}
	}
	# FIN SI HAY ITEMS

	if(!silente){
		cat('',fill=TRUE)
		cat('*** Esta es la cabecera de tus datos desapilados ***',fill=TRUE)
		cat('',fill=TRUE)
		print(head(dat.us))
	}
 return(dat.us)
}
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# Ordena los nombres de la interaccion por orden alfabetico b:a sera a:b
#---------------------------------------------------------------------------
 ordena.interac.fnc=function(efectos){
 	new.ef=strsplit(efectos,':')
	new.ef=lapply(new.ef,function(x)
		if(length(x) > 1) x=sort(x) else x=x )
	new.ef=lapply(new.ef, function(x){
		if(length(x) > 1){
		 ini=x[1]
		 for(i in 2:length(x)) ini=paste(ini,x[i],sep=':')
		 x=ini
		}else{
		 x=x
		}
        })
 efectos.ord=unlist(new.ef)
 return(efectos.ord)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# ANALISIS DE CORRESPONDENCIA.
# analisis.correspondencia.fnc(farms)
#---------------------------------------------------------------------------
 analisis.correspondencia.fnc=function(datos, es.tabla=FALSE,variables=NA, nd=NA, sup.fila=NA,
	sup.col=NA, que.dim=c(1,2), titulo=NA, xlim=NULL, ylim=NULL){
	AC=FALSE; ACM=FALSE
	require(ca, quietly = TRUE)
	hayplot=TRUE

	# SI NO TABLA
	if(!es.tabla){
		# SI NO NA VARIABLES
		if(!is.na(variables[1])){
			nombres=names(datos)
			if(is.numeric(variables)) variables=nombres[variables]
		}else{
			variables=names(datos)
		}
	} # FIN SI NO ES TABLA

	# SI DATOS ES TABLA AC SIMPLE
	if(es.tabla & is.na(variables[1])){
		print(contraste.ji.cuadrado.fnc(datos))
		crea.cat.fnc('Analisis de Correspondencia Simple')
		size=dim(datos)
		if(min(size) <= 2) {
		  nd=1
	 	  cat('', fill=TRUE)
		  cat('*** Warning. Dado que una de las variables tiene solo 2 niveles ',fill=TRUE)
	 	  cat('*** la solucion es unidimensional y no de creara grafico alguno ', fill=TRUE)
		  hayplot=FALSE
		}
		modelo.ca=ca(datos,nd)
		titulo=paste('AC Dim.',que.dim[1],' x ','Dim.',que.dim[2],sep='')
		AC=TRUE
	}

	# SI DATA.FRAME Y 2 VARIABLES. AC SIMPLE
	if(!es.tabla & length(variables)==2){
		datos=datos[,variables]
		nombres=names(datos)
		variables_=paste(nombres[1],':',nombres[2],sep='')
		tabla=frecuencias.fnc(datos,variables_, silente=TRUE)$tabla
		print(contraste.ji.cuadrado.fnc(datos, variables=variables))
		size=dim(tabla)
		crea.cat.fnc('Analisis de Correspondencia Simple')
		if(min(size) <= 2) {
		  nd=1
	 	  cat('', fill=TRUE)
		  cat('*** Warning. Dado que una de las variables tiene solo 2 niveles ',fill=TRUE)
	 	  cat('*** la solucion es unidimensional y no se creara grafico alguno', fill=TRUE)
		  hayplot=FALSE
		}
		modelo.ca=try(ca(as.table(tabla),nd),silent=TRUE)
		titulo=paste('AC Dim.',que.dim[1],' x ','Dim.',que.dim[2],variables,sep='')
		AC=TRUE
	}

	# SI DATA.FRAME Y > 2 VARIABLE. AC MULTIPLE
	if(!es.tabla & length(variables) > 2){
		crea.cat.fnc('Analisis de Correspondencia Multiple')
		modelo.ca=try(mjca(datos[,variables],nd=nd, lambda='indicator'),silent=TRUE)
		ACM=TRUE
		if(class(modelo.ca)=='try-error'){
			cat('',fill=TRUE)
			cat('*** Error. No se puede computar la descomposicion del valor singular de la matriz ***',fill=TRUE)
			cat('',fill=TRUE)
			stop( )
		}

		label=character( )
		for(i in 1:length(variables)) {
			if(i==1) label=paste(label,variables[i],sep='') else
				label=paste(label,variables[i],sep=':')
		}
		titulo=paste('ACM Dim.',que.dim[1],' x ','Dim.',que.dim[2],' ',label,sep='')
	}

	# SI DATA.FRAME Y NA VARIABLES. AC MULTIPLE
	if(!es.tabla & is.na(variables[1])){
		variables=names(datos)
		crea.cat.fnc('Analisis de Correspondencia Multiple')
		modelo.ca=mjca(datos[,variables],nd=nd, lambda='indicator')
		ACM=TRUE
		label=character( )
		for(i in 1:length(variables)) {
			if(i==1) label=paste(label,variables[i],sep='') else
				label=paste(label,variables[i],sep=':')
		}
		titulo=paste('ACM Dim.',que.dim[1],' x ','Dim.',que.dim[2],' ',label,sep='')
	}
	print(modelo.ca)

	if(is.na(nd)){
		cat('',fill=TRUE)
		cat('*** Si deseas un numero dado de dimensiones incluye el argumento ***',fill=TRUE)
		cat('*** nd igual al numero de dimensiones deseadas (Ej. nd=3)        ***',fill=TRUE)
		cat('',fill=TRUE)
		cat('*** Si deseas el grafico de otras dimensiones -por defecto se grafica',fill=TRUE)
		cat('*** las primeras dos- utiliza el argumento que.dim',fill=TRUE)
		cat('*** Ej: que.dim=c(1,3) ',fill=TRUE)
		cat('',fill=TRUE)		
	}
	if(hayplot){
		if(ACM){
			plot(modelo.ca, what=c('none','all'), dim=que.dim,main=titulo,
				xlim=xlim, ylim=ylim,
				col=c(c("black"),rainbow(length(modelo.ca$colnames))))
		}
		if(AC){
			plot(modelo.ca, what=c('all','all'), dim=que.dim,main=titulo,
				xlim=xlim, ylim=ylim,
			  col=c(c("black"),rainbow(length(modelo.ca$colnames))))
		}
	}
	try(detach(package:ca),silent=TRUE)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#
#---------------------------------------------------------------------------
dataframe.a.tabla.fnc=function(datos, variables=NA){
	if(is.na(variables[1])){
		cat(' ',fill=TRUE)
		cat('*** Error. Debes definir las dos variables correspondientes a la tabla ***',fill=TRUE)
 		cat("*** que has introducido. Ej. variables=c('partido','zona')             ***",fill=TRUE)
		cat(' ',fill=TRUE)
		stop( )
	}
	if(!is.data.frame(datos)){
		cat(' ',fill=TRUE)
		cat('*** Error. Los datos introducidos no pertenecen a la clase data.frame ***',fill=TRUE)
		cat(' ',fill=TRUE) 
		stop( )
	}
 	datos=as.table(as.matrix(datos))
 	names(dimnames(datos))=variables
		cat(' ',fill=TRUE)
		cat('*** Se ha transformado la data.frame introducida a la nueva clase tabla ***',fill=TRUE)
  return(datos)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# descriptivos.fnc(dat,variables=1:3)
# descriptivos.fnc(dat,variables=c('a','b'),traspuesta=T)
# descriptivos.fnc(dat,variables=23,factor='sexo')
#---------------------------------------------------------------------------
descriptivos.fnc=function(datos=NA,variables=NA, que.factor=NA, grafica=FALSE,
				lim=1, traspuesta=FALSE, vd=NA, silente=FALSE,angulo=0, simple=FALSE,
				size.font=0.8, como.lista=FALSE,orden=NULL, latex=FALSE,
				titulo=NULL, to.pdf=FALSE, ylim=NULL){

	if(class(try(is.na(datos)))=='logical') {
		cat('',fill=TRUE)
		crea.cat.fnc('descriptivos.fnc')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat("  descriptivos.fnc(datos)		                                        ",fill=TRUE)
		cat("  descriptivos.fnc(datos, variables=2:8, grafica=T)                     ",fill=TRUE)
		cat("  descriptivos.fnc(datos, variables=2:8, grafica=T, traspuesta=T)       ",fill=TRUE)
		cat("  descriptivos.fnc(datos, vd ='altura', que.factor='curso')             ",fill=TRUE)
		cat("  descriptivos.fnc(datos, vd ='altura', que.factor='curso:genero')      ",fill=TRUE)
		cat("  descriptivos.fnc(datos, vd ='altura', que.factor='curso', grafica=T)  ",fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Genera tabla de estadisticos descriptivos para las variables numericas ', fill=TRUE)
		cat(' definidas por el usuario en el argumento variables.                    ', fill=TRUE)
		cat(' Si se utiliza el argumento vd puede entonces incluirse el argumento ', fill=TRUE)
		cat(' que factor con el cruce de hasta 5 variables cualitativas.             ', fill=TRUE)
		cat(" ", fill=TRUE)
		cat(" sites.google.com/site/ullrtoolbox/estadisticos-descriptivos/descriptivos-fnc", fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		print(argumentos('descriptivos.fnc'))
	return('help')
	}

  require(psych, quietly = TRUE)
  require(lattice, quietly = TRUE)

  if(grafica){
    library(RColorBrewer)
    #display.brewer.all() 
    myColours <- brewer.pal(6,"Blues")
    my.settings <- list(
    superpose.polygon=list(col=myColours[2:6], border="transparent"),
    strip.background=list(col=myColours[3]),
    strip.border=list(col="black"))  
  }
  
	dat=datos
	nombresvar=names(dat)

	check.que.factor(que.factor)

	# EXISTEN LAS VARIABLES
	if(!is.na(variables[1])){
	  	if(is.numeric(variables)){
	    		nombres=names(datos)
	    		variables=nombres[variables]
	  	}
	  	hay.dos.puntos=length(strsplit(variables,':')[[1]]) != 1
	  	if(hay.dos.puntos) {
			cat('',fill=TRUE)
			cat("*** Error. El argumento variables no admite ese formato: 'A:B' en la funcion descriptivos.fnc",fill=TRUE)
			cat('*** Elimina ese argumento y sustituyelo por el argumento que.factor',fill=TRUE)
			cat("*** Ej. descriptivos.fnc(mis.datos, vd='peso', que.factor='A:B') ",fill=TRUE)
			cat('*** Necesitaras ademas incluir el argumento vd si no existe una variable',fill=TRUE)
			cat('*** con ese nombre en la base de datos introducida.',fill=TRUE)
			cat('',fill=TRUE)
			stop( )
	  	}
		chivato=existe.variable.fnc(datos,variables)
		if(chivato$cc !=0){
			cat('',fill=TRUE)
			if(chivato$cc > 1){
			 cat('*** Error. Las variables:',chivato$faltan,'no existen en la base de datos ***',fill=TRUE)
	       	 cat('',fill=TRUE)
			}else{
			 cat('*** Error. La variable:',chivato$faltan,'no existe en la base de datos ***',fill=TRUE)
	       	 cat('',fill=TRUE)
			}
		stop( )
		}
	}
	# FIN SI EXISTEN LAS VARIABLES

	# EXISTEN LOS FACTORES
	if(!is.na(que.factor[1])){
		chivato=existe.variable.fnc(datos,que.factor)
		if(chivato$cc !=0){
			cat('',fill=TRUE)
			if(chivato$cc > 1){
			 cat('*** Error. Los factores:',chivato$faltan,'no existen en la base de datos ***',fill=TRUE)
	       	 cat('',fill=TRUE)
			}else{
			 cat('*** Error. El factor:',chivato$faltan,'no existe en la base de datos ***',fill=TRUE)
	       	 cat('',fill=TRUE)
			}
		stop( )
		}
	}
	# FIN SI EXISTEN FACTORES

  # CHECK SI QUE.FACTOR
  if(!is.na(que.factor[1])){
	n.fac=strsplit(que.factor,':')[[1]]
	nn.fac=length(n.fac)
	for(i in 1:nn.fac){
		chiva=table(dat[,n.fac[i]])==0
		que.niveles=levels(dat[,n.fac[i]])
		que.niveles=que.niveles[!chiva]
		if(sum(chiva) !=0) dat[,n.fac[i]]=factor(as.character(dat[,n.fac[i]]),
								levels=que.niveles)
	}
  }
  # FIN SI QUE.FACTOR

  # SI NO SE DECLARAN VARIABLES NI VD NI QUE.FACTOR ENTRAN TODAS
  if(is.na(variables[1]) & is.na(vd[1]) & is.na(que.factor[1])){
    variables=names(dat)
  } # FIN SI NO SE DECLARA VARIABLE ALGUNA

  # SI NA.VARIABLES NA.VD Y HAY FACTOR O FACTORES
  if(is.na(variables[1]) & is.na(vd[1]) & !is.na(que.factor[1])){
	if(nn.fac==1){
    		variables=names(dat)
		que.col=match(que.factor, variables)
		if(is.na(que.col[1])){
			cat('*** Error. No existe en la base de datos el factor',que.factor,'***',fill=TRUE)
			stop( )
		}else{
			variables=variables[-que.col]
		}
		if('vd' %in% variables) {
		  vd='vd'; variables=NA
		}else{
		  chivato=dime.si.hay.factores.fnc(data.frame(dat[,variables]))
		  if(length(chivato$factores) != 0){
		    cat('',fill=TRUE)
		    cat('*** Error. La o las siguientes variables no son numericas. No pueden calcularse estadisticos',fill=TRUE)
		    cat('*** de resumen, media, mediana, etc de variables alfanumericas. Estas son las variables:',fill=TRUE)
		    cat('*** variable: ',chivato$factores,' columna: ',chivato$columnas,fill=TRUE)
		    cat('',fill=TRUE)
		    cat('',fill=TRUE)
		    stop( )
		   }
		}
	}
	
	if(nn.fac > 1 & is.na(vd[1])){
		nombresvar=names(dat)
		hay.vd=match('vd',nombresvar)
		vd='vd'
		if(is.na(hay.vd)) {
			cat('',fill=TRUE)
			cat('*** Error. Has indicado varios factores y no has incluido el argumento vd  ***',fill=TRUE)
			cat('*** Si el numero de factores es superior a 1 debes incluir dicho argumento ***',fill=TRUE)
			cat('*** si no existe una variable con nombre vd en la base de datos.           ***',fill=TRUE)
			cat("*** descriptivos.fnc(mis.datos, vd='altura', que.factor='A:B')             ***",fill=TRUE)
			cat('',fill=TRUE)
		stop( )
		}
	}
  } # FIN SI NA.VARIABLES NA.VD Y HAY FACTOR O FACTORES

  # SI VD NUMERICA SE TRANSFORMA. SI NO EXISTE LA VD DECLARADA RETURN
  nombresvar=names(dat)
  if(!is.na(vd[1])){
	if(length(vd) > 1){
	  cat('',fill=TRUE)
	  cat('*** Error. El argumento vd no admite mas de una variable. Utilizas vd en lugar del ***',fill=TRUE)
	  cat('*** variables porque deseas los descriptivos media, dt y n de una sola variable en ***',fill=TRUE)	
	  cat('*** el cruce de dos o mas factores. Puedes sustituir vd por el argumento variables ***',fill=TRUE)	
	  cat('*** si solo tienes un factor.                                                      ***',fill=TRUE)	
	  cat("*** Ej:  descriptivos.fnc(datos, variables=1:3, que.factor='tratamiento')          ***",fill=TRUE)	
	  cat('',fill=TRUE)
	  stop( )  
	}
	if(is.numeric(vd)) vd=nombresvar[vd]
	existe.vd=match(vd,nombresvar)
	if(is.na(existe.vd[1])){
		cat('',fill=TRUE)
		cat('*** Error. Has introducido una vd:',vd,'que no existe. ***',fill=TRUE)
		stop( )
	}
  }
  # FIN SI VD NUMERICA

  # CHECK SI HAY VARIABLES ALFANUMERICAS
  if(!is.na(variables[1])){
  	chivato=dime.si.hay.factores.fnc(data.frame(datos[,variables]))
  	if(length(chivato$factores) != 0){
	 cat('',fill=TRUE)
	 cat('*** Error. Las siguientes variables no son numericas. No pueden calcularse estadisticos  ***',fill=TRUE)
	 cat('*** de resumen, media, mediana, etc de variables alfanumericas. Estas son las variables: ***',fill=TRUE)
	 cat('*** ',chivato$factores,fill=TRUE)
	 cat('',fill=TRUE)
	 print(nombresvar(datos[,chivato$factores]))
	 cat('',fill=TRUE)
	 stop( )
  	}
  }
  # FIN CHECK SI HAY VARIABLES ALFANUMERICAS

  nombres=c('var','n','media','dt','mediana','trimedia','mad','min','max',
	'rango','asimetria','apuntamiento','et')
  nombresvar=names(dat)

  if(grafica & is.na(vd[1])){
      cat('',fill=TRUE)
      cat('*** WARNING. No se genera grafica alguna si no existe el argumento vd. Si utilizas     ***',fill=TRUE)
      cat('*** el argumento variables es porque deseas multiples estadisticos de resumen en un    ***',fill=TRUE)
      cat('*** factor como maximo. El argumento grafica=T entiende que deeas la grafica de medias ***',fill=TRUE)
      cat('*** para la variable incluida en el argumento vd. ***',fill=TRUE)
      cat('',fill=TRUE)
 }      
  
  # SI VD ENTONCES DESC X FACTOR
  hay.vd=match(vd,nombresvar)
  if(is.na(variables[1]) & !is.na(que.factor[1]) & !is.na(hay.vd)){
    	if(!silente) crea.cat.fnc('ESTADISTICOS DESCRIPTIVOS')
	lista=list( )
 	for(i in 1:nn.fac){
		tabla=do.call(cbind,tabla.descriptivos.fnc(dat,que.efecto=n.fac[i],vd=vd))
		if(!traspuesta) tabla=t(tabla)
		lista[[i]]=tabla
	}
	names(lista)=n.fac
	if(grafica & nn.fac==1){
		if(min(tabla[1,])>0) inf=0 else inf=min(tabla[1,])
		maximo=max(tabla[1,]);
		if(maximo < 1){
			maximo=maximo+(maximo*(lim/10))
		}else{
			maximo=ceiling(maximo+(maximo*(lim/10)))
		}
		limites=c(inf,maximo)
		desc=descriptivos.fnc(datos, vd=vd, que.factor=que.factor, silente=T)
		if(is.null(ylim[1])){
		  grafi=barchart(as.table(desc[[1]][1,]), col='gray',layout=orden,
				scales = list(x = list("free",rot=angulo,cex=size.font)),
				stack=FALSE, horizontal=F, ylab=paste('media: ',vd,sep=''),
				xlab=que.factor, auto.key = list(title = que.factor),
				par.settings = my.settings)
		}else{
		  grafi=barchart(as.table(desc[[1]][1,]), col='gray',layout=orden, ylim=ylim,
				scales = list(x = list("free",rot=angulo,cex=size.font)),
				stack=FALSE, horizontal=F, ylab=paste('media: ',vd,sep=''),
				xlab=que.factor, auto.key = list(title = que.factor),
				par.settings = my.settings)		
		}
		if(!to.pdf) x11()
		print(grafi)
		#barplot(tabla[1,],ylim=limites,ylab=vd,main=que.factor)
		#box( )
	}

	if(nn.fac!=1){
		desc = tabla.descriptivos.fnc(dat,que.efecto=que.factor,vd=vd)
		lista=c(lista,desc)
		if(grafica){
			vi1=n.fac[1]; vi2=n.fac[2]
			modelo=paste(vd,'~',vi1,'|',vi2,sep='')
			if(nn.fac==2){
				if(is.null(ylim[1])){
				  grafi=barchart(desc[[1]]$media, scales = list(x = list("free",rot=angulo,cex=size.font)),
					stack=FALSE,layout=orden, main=titulo,
					horizontal=F, ylab=paste('media ',vd,sep=''), xlab=vi1,
     					auto.key = list(title = vi2, space='right'),
     					par.settings = my.settings)
     				}else{
				  grafi=barchart(desc[[1]]$media, 
					scales = list(x = list("free",rot=angulo,cex=size.font)),
					stack=FALSE,layout=orden, main=titulo,ylim=ylim,
					horizontal=F, ylab=paste('media ',vd,sep=''), xlab=vi1,
     					auto.key = list(title = vi2, space='right'),
     					par.settings = my.settings)     				
     				}
			}
			if(nn.fac==3){
				vi3=n.fac[3]
				if(vd=='vd'){
					tabla=with(dat, tapply(dat[,'vd'],
						list(eval(parse(text=vi1)),eval(parse(text=vi2)),eval(parse(text=vi3))),
						function(x) mean(x,na.rm=T)))
				}else{
					tabla=with(dat, tapply(dat[,vd],
						list(eval(parse(text=vi1)),eval(parse(text=vi2)),eval(parse(text=vi3))),
						function(x) mean(x,na.rm=T)))
				}
				if(is.null(ylim[1])){
				  grafi=barchart(tabla, scales = list(x = list("free",rot=angulo,cex=size.font)),
					stack=FALSE,layout=orden, main=titulo,
					horizontal=F, ylab=paste('media ',vd,sep=''), xlab=vi1,
     					auto.key = list(title = vi3, space='right'),
     					par.settings = my.settings)
				}else{
				  grafi=barchart(tabla, scales = list(x = list("free",rot=angulo,cex=size.font)),
					stack=FALSE,layout=orden, main=titulo, ylim=ylim,
					horizontal=F, ylab=paste('media ',vd,sep=''), xlab=vi1,
     					auto.key = list(title = vi3, space='right'),
     					par.settings = my.settings)				  
     				}	
			}
			if(nn.fac==4){
				vi3=n.fac[3];	vi4=n.fac[4]
				if(vd=='vd'){
					tabla=with(dat, tapply(dat[,'vd'],
						list(eval(parse(text=vi1)),eval(parse(text=vi2)),
							eval(parse(text=vi3)),eval(parse(text=vi4))),
						function(x) mean(x,na.rm=T)))
				}else{
					tabla=with(dat, tapply(dat[,vd],
						list(eval(parse(text=vi1)),eval(parse(text=vi2)),
							eval(parse(text=vi3)),eval(parse(text=vi4))),
						function(x) mean(x,na.rm=T)))
				}
				if(is.null(ylim[1])){				
				  grafi=barchart(tabla, scales = list(x = list("free",rot=45,cex=size.font)),
					stack=FALSE,layout=orden, main=titulo,
					horizontal=F, ylab=paste('media ',vd,sep=''), xlab=vi1,
     					auto.key = list(title = vi4, space='right'),
     					par.settings = my.settings)
     				}else{
				  grafi=barchart(tabla, scales = list(x = list("free",rot=45,cex=size.font)),
					stack=FALSE,layout=orden, main=titulo, ylim=ylim,
					horizontal=F, ylab=paste('media ',vd,sep=''), xlab=vi1,
     					auto.key = list(title = vi4, space='right'),
     					par.settings = my.settings)     				
     				}
			}
			if(!to.pdf) x11()
			print(grafi)
		}
	}
	try(detach(package:psych),silent=TRUE)
    if(is.na(variables[1]) & !silente) cat('*** VD:',vd,fill=TRUE)
    cat('',fill=TRUE)
    if(latex) latex.fnc(lista)    
    return(lista)
  }
  # FIN SI VD ENTONCES DESC X FACTOR

  # SI NO HAY VARIABLES NI FACTOR Y HAY VD
  if(is.na(variables[1]) & !is.na(hay.vd[1])){
    if(!silente) crea.cat.fnc('ESTADISTICOS DESCRIPTIVOS')
	# ERROR SI NO HAY QUE.FACTOR Y SI VD
	if(is.na(que.factor[1])){
		cat('',fill=TRUE)
		cat('*** Error. Si defines el argumento vd debes introducir el argumento que.factor ***',fill=TRUE)
		cat('*** Reformula tu solicitud incluyendo el argumento variables y elimina vd	    ***',fill=TRUE)
		cat("*** Ej. descriptivos.fnc(mis.datos, variables=1:8)                              ***",fill=TRUE)
		cat("*** Ej. descriptivos.fnc(mis.datos, variables=c('m1','m2','m3'), que.factor='zona') ***",fill=TRUE)
		cat('',fill=TRUE)
		stop( )
	}
	# FIN ERROR SI NO HAY QUE FACTOR Y SI VD
    desc = tabla.descriptivos.fnc(dat,que.efecto=que.factor,vd=vd)
	if(nn.fac== 1){
		desc=do.call(cbind, desc)
		if(traspuesta) desc=t(desc)
	}
    if(is.na(variables[1]) & !silente) cat('*** VD: ',vd,fill=TRUE)
    cat('',fill=TRUE)
    if(latex) latex.fnc(desc)    
    return(desc)
  }
  # FIN SI NO HAY VARIABLES Y FACTOR Y SI VD

  # SI SE SOLICITA DESCRIPTIVOS X NIVELES DE UN FACTOR
  if(!is.na(que.factor[1])){

     # if(!silente) crea.cat.fnc('ESTADISTICOS DESCRIPTIVOS')

      n.fac=strsplit(que.factor,':')
      nn.fac=length(n.fac[[1]])

	if(!is.na(hay.vd[1]) & is.na(variables[1]) & !silente) cat('*** VD: ',vd,fill=TRUE)

	# SI NA VARIABLES NO VD Y N.FAC=1 ENTONDES VARIABLES SON TODAS
	if(is.na(hay.vd[1]) & is.na(variables[1]) & nn.fac==1){
		que.col=match(que.factor,nombresvar)
		variables=nombresvar[-que.col]
	}
	# FIN SI NA VARIABLES

      # SI NO VD Y N.FAC > 1
      if(is.na(vd[1]) & nn.fac > 1){
	cat('',fill=TRUE)
	cat('*** Error. Cuando incluyes el argumento variables el numero de factores no puede ser   ***',fill=TRUE)
	cat('*** superior a uno. Elimina un factor del argumento que.factor, o incluye el argumento ***',fill=TRUE)
	cat('*** vd en lugar del argumento variables.                                               ***',fill=TRUE)
	cat("*** Ej.  descriptivos.fnc(datos, vd='peso', que.factor='sexo:raza'                     ***",fill=TRUE)
	cat('',fill=TRUE)
      stop( )
      } # FIN
      # SI VARIABLES Y N.FAC > 1
      if(!is.na(variables[1]) & nn.fac > 1){
	cat('',fill=TRUE)
	cat('*** Error. Cuando incluyes el argumento variables el numero de factores no  puede ser       ***',fill=TRUE)
	cat('*** superior a uno. Elimina un factor del argumento que.factor, o incluye el argumento ***',fill=TRUE)
	cat('*** vd en lugar del argumento variables.                                               ***',fill=TRUE)
	cat("*** Ej.  descriptivos.fnc(datos, vd='peso', que.factor='sexo:raza'                     ***",fill=TRUE)
	cat('',fill=TRUE)
      stop( )
      } # FIN

      if(length(n.fac) > 1) {
	cat('',fill=TRUE)
	cat('*** Error. El argumento que.factor no puede contener mas de un factor, si se desea            ***',fill=TRUE)
	cat('*** estadisticos de resumen para multiples variables (argumento variables). Si quieres una    ***',fill=TRUE)
	cat('*** tabla para una determinada variable dependiente en el cruce de dos o tres factores,       ***',fill=TRUE)
	cat('*** utiliza como valores de que.factor, los que desees (maximo 3) separados por el caracter   ***',fill=TRUE)
	cat("*** dos puntos (:) (Ej. que.factor= 'sexo:zona'). En este caso debes incluir el argumento vd  ***",fill=TRUE)
	cat('*** indicando la variable dependiente sobre la que deseas los estadisticos descriptivos       ***',fill=TRUE)
	cat('*** del cruce anterior.                                                                       ***',fill=TRUE)
      stop( )
      }
  } # FIN SI HAY QUE.FACTOR


  # CHEQUEA SI EL ARGUMENTO VARIABLES ES NUMERICO O NO Y GENERA EL NOMBRE DE LAS
  # FILAS DE LA SALIDA
  if(is.numeric(variables)){
      nombresvar=nombresvar[variables]
  }else{
      nombresvar=variables
  } # FIN

  # SI DESCRIPTIVOS POR FACTOR O GENERALES
  if(is.na(que.factor[1])) {

      if(!silente) crea.cat.fnc('ESTADISTICOS DESCRIPTIVOS')

      if(!is.na(variables[1]) & !is.na(vd[1])) {
		cat('',fill=TRUE)
		cat('*** Atencion. Has incluido dos argumentos incompatibles: variables y vd, se utilizara solo ***',fill=TRUE)
		cat('*** el primero (variables). Si deseas que sea vd, elimina variables de la lista.           ***',fill=TRUE)
		cat('',fill=TRUE)
      } # FIN DE SI VARIABLES Y VD SIMULTANEAMENTE

      desc=describe(dat[,variables])
      names(desc)=nombres
      desc=desc[,-1]
      row.names(desc)=nombresvar
      if(simple) desc=desc[,c(2,3,1)]
      if(traspuesta) desc=t(desc)

  }else{ # SI HAY FACTORES

      if(!silente){ crea.cat.fnc('ESTADISTICOS DESCRIPTIVOS')
      cat('*** Por niveles de la variable: ',que.factor,fill=TRUE)
      cat('',fill=TRUE)
	}

    if(!is.na(variables[1]) & !is.na(vd[1])) {
     cat('',fill=TRUE)
     cat('*** Atencion. Has incluido dos argumentos incompatibles: variables y vd, se utilizara solo ***',fill=TRUE)
     cat('*** el primero (variables). Si deseas que sea vd, elimina variables de la lista.           ***',fill=TRUE)
     cat('',fill=TRUE)
    } # FIN DE SI VARIABLES Y VD SIMULTANEAMENTE

    	desc=describe.by(dat[,nombresvar],dat[,que.factor])
    	desc=unclass(desc)
    	desc=lapply(desc, function(x) {
		names(x)=nombres; row.names(x)=nombresvar; x=x[,-1]; return(x)})
	if(simple) desc=lapply(desc, function(x) x=x[,c(2,3,1)])
	
	if(!como.lista){
 		n.niveles=length(desc)
 		estad=names(desc[[1]])
 		n.carac=nchar(names(desc))
 		que.min=min(n.carac)
 		if(que.min ==3) inicial=substr(names(desc),1,3)
 		if(que.min >=4) inicial=substr(names(desc),1,4)
 		if(que.min <=2) inicial=substr(names(desc),1,2)
 		desc=round(do.call(cbind,desc),3)
 		if(simple){
			lista = data.frame(matrix(1:(3*n.niveles),nrow=n.niveles, byrow=TRUE))
 		}else{
			lista = data.frame(matrix(1:(12*n.niveles),nrow=n.niveles, byrow=TRUE))
		}
 		lista=stack(lista)$values
 		desc=desc[,lista]
 		acumu=character()
 		for(i in inicial) acumu= rbind(acumu,paste(estad,i,sep='.'))
 		names(desc)=acumu
 		head(desc)
    		if(traspuesta) desc=data.frame(t(desc))
	}else{
		if(traspuesta) desc=lapply(desc, function(x) x=t(x))
	}
  } # FIN DE DESCRIPTIVOS POR FACTOR O GENERALES
  
 if(latex) latex.fnc(desc)  
 return(desc)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#  prueba.levene.fnc(dat=terapia,vd='autoestima',fac.inter='grupo')
#---------------------------------------------------------------------------
 prueba.levene.fnc=function(datos,vd=NA,fac.inter){
	check.factores.fnc(fac.inter,fac.intra=NA)

	dat=datos
	require(car, quietly = TRUE)
	if(is.na(vd[1]) ) vd='vd'

	n.fac=length(fac.inter)
	if(n.fac==1){
	 if(vd=='vd') f1=with(dat,leveneTest(dat$vd,eval(parse(text=fac.inter[1])))) else
 	 f1=leveneTest(dat[,vd], dat[,fac.inter[1]])
	 row.names(f1)=c(fac.inter[1],'')
	 lista=list(prueba.levene=f1)
	}
	if(n.fac==2){
	 if(vd =='vd') {
	    f1=with(dat,leveneTest(dat$vd, eval(parse(text=fac.inter[1]))))
	    row.names(f1)=c(fac.inter[1],'')
	    f2=with(dat,leveneTest(dat$vd, eval(parse(text=fac.inter[2]))))
	    row.names(f2)=c(fac.inter[2],'')
	    interac=paste(fac.inter[1],':',fac.inter[2],sep='')
	    f3=with(dat, leveneTest(dat$vd, interaction(eval(parse(text=fac.inter[1])),
			eval(parse(text=fac.inter[2]))) ) )
	    row.names(f3)=c(interac,'')
	 }else{
	    f1=leveneTest(dat[,vd], dat[,fac.inter[1]])
	    row.names(f1)=c(fac.inter[1],'')
	    f2=leveneTest(dat[,vd], dat[,fac.inter[2]])
	    row.names(f2)=c(fac.inter[2],'')
	    interac=paste(fac.inter[1],':',fac.inter[2],sep='')
	    f3=with(dat, leveneTest(dat[,vd], interaction(eval(parse(text=fac.inter[1])),
			eval(parse(text=fac.inter[2]))) ) )
	    row.names(f3)=c(interac,'')
	 }
	 lista=list(prueba.levene=f1,prueba.levene=f2,prueba.levene=f3)
	}
	if(n.fac==3){
	    interac1=paste(fac.inter[1],':',fac.inter[2],sep='')
	    interac2=paste(fac.inter[1],':',fac.inter[3],sep='')
	    interac3=paste(fac.inter[2],':',fac.inter[3],sep='')
	 if(vd =='vd') {
	    f1=with(dat,leveneTest(dat$vd, eval(parse(text=fac.inter[1]))))
	    row.names(f1)=c(fac.inter[1],'')
	    f2=with(dat,leveneTest(dat$vd, eval(parse(text=fac.inter[2]))))
	    row.names(f2)=c(fac.inter[2],'')
	    f3=with(dat,leveneTest(dat$vd, eval(parse(text=fac.inter[3]))))
	    row.names(f3)=c(fac.inter[3],'')
	    f4=with(dat, leveneTest(dat$vd, interaction(eval(parse(text=fac.inter[1])),
			eval(parse(text=fac.inter[2]))) ) )
	    row.names(f4)=c(interac1,'')
	    f5=with(dat, leveneTest(dat$vd, interaction(eval(parse(text=fac.inter[1])),
			eval(parse(text=fac.inter[3]))) ) )
	    row.names(f4)=c(interac2,'')
	    f6=with(dat, leveneTest(dat$vd, interaction(eval(parse(text=fac.inter[2])),
			eval(parse(text=fac.inter[3]))) ) )
	    row.names(f5)=c(interac3,'')
	 }else{
	    f1=leveneTest(dat[,vd], dat[,fac.inter[1]])
	    row.names(f1)=c(fac.inter[1],'')
	    f2=leveneTest(dat[,vd], dat[,fac.inter[2]])
	    row.names(f2)=c(fac.inter[2],'')
	    f3=leveneTest(dat[,vd], dat[,fac.inter[3]])
	    row.names(f3)=c(fac.inter[3],'')
	    f4=with(dat, leveneTest(dat[,vd], interaction(eval(parse(text=fac.inter[1])),
			eval(parse(text=fac.inter[2]))) ) )
	    row.names(f4)=c(interac1,'')
	    f5=with(dat, leveneTest(dat[,vd], interaction(eval(parse(text=fac.inter[1])),
			eval(parse(text=fac.inter[3]))) ) )
	    row.names(f4)=c(interac2,'')
	    f6=with(dat, leveneTest(dat[,vd], interaction(eval(parse(text=fac.inter[2])),
			eval(parse(text=fac.inter[3]))) ) )
	    row.names(f5)=c(interac3,'')
	 }
	 lista=list(prueba.levene=f1,prueba.levene=f2,prueba.levene=f3,prueba.levene=f4,
		prueba.levene=f5, prueba.levene=f6)
	}
	#try(detach(package:car),silent=TRUE)
 return(lista)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# potencia.planeada.anova.fnc(eta2=0.6, niveles=3)
#---------------------------------------------------------------------------
 potencia.planeada.anova.fnc=function(eta2, niveles, alfa=NA, potencia=NA, mr=FALSE){
	crea.cat.fnc('Potencia Planeada')
	if(!mr) {
		cat('*** Para medidas repetidas utiliza el argumento mr=T ***',fill=TRUE)
		cat('',fill=TRUE)
	}

	if(is.na(alfa[1])) alfa=0.05
	if(is.na(potencia[1])) potencia=0.80
	delta=sqrt( eta2/ (0.25 * (1-eta2)))
	medias=rep(0,niveles); medias[1]=delta
	dif=1; n=4
	gl1=niveles-1
	while (dif > 0.01) {
		gl2=(n*niveles)-niveles
		pnc= n*sum((medias-mean(medias))^2)/1
		F.h0.95=qf(1-alfa, gl1,gl2)
		pow.est= 1 - pf(F.h0.95, gl1,gl2,pnc)
		dif=potencia-pow.est; dif
		n=n+1
	}
	if(mr) n=(n-1)/1.83
	if(mr) tipo='medidas.repetidas' else tipo='intergrupo'
 return(list(pow.planeada=potencia, eta2=eta2,alfa=alfa, tipo=tipo, n.condi=n-1))
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# diagrama.cajas.fnc(dat,vd=NA,factores,identifica=NA)
#---------------------------------------------------------------------------
 diagrama.cajas.fnc=function(datos,vd=NA,que.factor=NA,identifica=NA, corte.dt=NA, titulo=NA, size.font=1){
	dat=datos
	if(!is.na(corte.dt[1])) {
		identifica=NA
		cat('',fill=TRUE)
		cat('*** Warning. Si incluyes el argumento corte.dt se desactiva el procedimiento',fill=TRUE)
		cat('*** de indentificacion de casos atipicos y extremos',fill=TRUE)
		cat('',fill=TRUE)
	}		
	check.que.factor(que.factor)

	require(lattice, quietly = TRUE)
	if(!is.na(vd[1])){
		if(is.numeric(vd)){
			nombres=names(datos)
			vd=nombres[vd]
		}
	}

	# EXISTE FACTORES DE QUE.FACTORES
	if(!is.na(que.factor[1])){
	  hay.dosp=grep(':',que.factor)
	  if(length(hay.dosp)==1){
	    chivato=strsplit(que.factor,':')[[1]]
		# EXISTEN LAS VARIABLES
		check=existe.variable.fnc(datos,chivato)
		if(check$cc !=0) {
			cat('',fill=TRUE)
			cat('*** Error. No existe la variable o variables:',check$faltan,fill=TRUE)
			cat('*** en la base de datos incluida. Revisa los argumentos',fill=TRUE)
			stop( )
		}
            nn.fac=length(chivato)
	    if(nn.fac > 4) {
	      cat('',fill=TRUE)
	      cat('*** Error. El numero maximo de factores es 4 ***',fill=TRUE)
	      cat('',fill=TRUE)
	      stop( )
	    }

	    if(nn.fac==2){
# 		dat=subset(dat, !is.na(eval(parse(text=chivato[1]))) & 
# 		  !is.na(eval(parse(text=chivato[2]))))
# 		dat$condi=paste(dat[,chivato[1]],dat[,chivato[2]],sep='.')
		fac.inter=c(chivato[1],chivato[2])
		dat=crea.condicion.fnc(dat, fac.inter=fac.inter,silente=TRUE)
	    }
	    if(nn.fac==3){
# 		dat=subset(dat, !is.na(eval(parse(text=chivato[1]))) & 
# 		  !is.na(eval(parse(text=chivato[2]))) &
# 		  !is.na(eval(parse(text=chivato[3]))) )	    
# 		dat$condi=paste(dat[,chivato[1]],dat[,chivato[2]],dat[,chivato[3]],sep='.')
		dat=crea.condicion.fnc(dat, 
		  fac.inter=c(chivato[1],chivato[2],chivato[3]),silente=TRUE)
	    }
	    if(nn.fac==4){
# 		dat=subset(dat, !is.na(eval(parse(text=chivato[1]))) & 
# 		  !is.na(eval(parse(text=chivato[2]))) &
# 		  !is.na(eval(parse(text=chivato[3]))) &
# 		  !is.na(eval(parse(text=chivato[4]))) )	    	    
# 		dat$condi=paste(dat[,chivato[1]],dat[,chivato[2]],
# 			    dat[,chivato[3]],dat[,chivato[4]],sep='.')
		dat=crea.condicion.fnc(dat, 
		    fac.inter=c(chivato[1],chivato[2],chivato[3],chivato[4]),silente=TRUE)
	    }
# 	    dat$condi=factor(dat$condi)
	    que.factor='condicion'
	  }else{	
	  # CHECK SI EXISTEN LAS VARIABLES
	  chivato=match(que.factor, names(datos))
	  if(sum(is.na(chivato)) > 0) {
	    cat('',fill=TRUE) 
	    cat('*** Error. No existe la variable', variables[is.na(chivato)],'en la base de datos ***',fill=TRUE)
	    stop( )
	  }
	}
      }
 
      # FIN SI CHECK VARIABLES

	if(is.na(vd[1])){
		chivato=match('vd', names(dat))
		if(!is.na(chivato[1])){
			vd='vd'
		}else{
			cat(' ',fill=TRUE)
			cat('*** Error, No has introducido datos apilados y no has declarado   ***',fill=TRUE)
			cat('*** el argumento vd en la llamada a la funcion.                   ***',fill=TRUE)
			cat("*** Ej. diagrama.cajas.fnc(mis.datos, vd='f1', que.factor='zona') ***",fill=TRUE)
		 stop( )
		}
	} # CIERRE NA VD

	if(is.na(que.factor[1])) n.fac=0 else n.fac=length(que.factor)

	if(!is.na(corte.dt[1])) dat=dat[tipifica.fnc(dat,vd) <=corte.dt &
				    tipifica.fnc(dat,vd) >=(corte.dt*-1),]
	
	# COMIENZAN LOS BOXPLOT
	if(n.fac==0){
		if(is.na(titulo[1])) titulo=vd
		if(is.na(corte.dt[1])){
		    boxplot(dat[,vd],data=dat,main=titulo, cex.axis=size.font)
		}else{
		    boxplot(dat[,vd],data=dat,main=titulo, cex.axis=size.font,
		      ylab=paste(vd,' dentro +-',corte.dt,' dt',sep=''))
		}
		if(!is.na(identifica[1])){
		 cat('*** Presiona esc cuando termines la identificacion ***',fill=TRUE)
		 identify(rep(1,dim(dat)[1]),dat[,vd])
		}
	}
	if(n.fac > 2){
	 cat('',fill=TRUE)
	 cat("*** Ej. diagrama.cajas.fnc(mis.datos, vd='f1', que.factor='zona') ***",fill=TRUE)
	 cat('*** Error. El numero maximo de factores debe ser 2. Elimina algun factor ***',fill=TRUE)
	 cat('*** del argumento que.factor.                                              ***',fill=TRUE)
 	 stop( )
	}
	if(n.fac==1){
		if(is.na(titulo[1])) titulo=paste(vd,' por ',que.factor,sep='')
 		modelo=paste(vd,'~',que.factor)
		if(is.na(corte.dt[1])){
		  boxplot(eval(parse(text=modelo)),data=dat, cex.axis=size.font, main=titulo)
		}else{
		  boxplot(eval(parse(text=modelo)),data=dat, cex.axis=size.font,
		    ylab=paste(vd,' dentro +-',corte.dt,' dt',sep=''), main=titulo)
		}
		if(!is.na(identifica[1])){
		 cat('*** Presiona esc cuando termines la identificacion ***',fill=TRUE)
		 identify(as.numeric(dat[,que.factor]),dat[,vd])
		}
	} # FIN SI 1 FACTOR
	if(n.fac==2){
 		modelo1=paste(vd,'~',que.factor[1])
 		modelo2=paste(vd,'~',que.factor[2])
		# CREAMOS ORDEN COHERENTE A LOS NIVELES DE LA INTERACCION
		niv.A=levels(dat[,que.factor[1]])
		niv.B=levels(dat[,que.factor[2]])
		alma=character( )
		for(j in 1:length(niv.A))
			for(i in 1:length(niv.B))
				alma=cbind(alma,paste(niv.A[j],niv.B[i],sep='.'))
		niveles=as.character(alma)
		# FIN ORDEN NIVELES
 		dat$interac=factor(paste(dat[,que.factor[1]],dat[,que.factor[2]],sep='.'),
					levels=niveles)
 		modelo3=paste(vd,'~','interac')

		if(is.na(corte.dt[1])){
		  if(is.na(titulo[1])) titulo=paste(vd,' por ',que.factor[1],sep='')
		  boxplot(eval(parse(text=modelo1)),data=dat, cex.axis=size.font, main=titulo)
		  X11( )
		  if(is.na(titulo[1])) titulo=paste(vd,' por ',que.factor[2],sep='')		  
		  boxplot(eval(parse(text=modelo2)),data=dat, cex.axis=size.font, main=titulo)
		  X11( )
		  if(is.na(titulo[1])) titulo=paste(vd,' por ',que.factor[1],':',que.factor[2],sep='')		  
		  boxplot(eval( parse(text=modelo3)),data=dat, cex.axis=size.font, main=titulo)
		}else{
		  if(is.na(titulo[1])) titulo=paste(vd,' por ',que.factor[1],sep='')
		  boxplot(eval(parse(text=modelo1)),data=dat, cex.axis=size.font,
		    main=paste(vd,' por ',que.factor[1],sep=''),
		    ylab=paste(vd,' dentro +-',corte.dt,' dt',sep='') )
		  X11( )
		  if(is.na(titulo[1])) titulo=paste(vd,' por ',que.factor[2],sep='')		  		  
		  boxplot(eval(parse(text=modelo2)),data=dat, cex.axis=size.font,
		    main=paste(vd,' por ',que.factor[2],sep=''),
		    ylab=paste(vd,' dentro +-',corte.dt,' dt',sep='') )
		  X11( )
		  if(is.na(titulo[1])) titulo=paste(vd,' por ',que.factor[1],':',que.factor[2],sep='')		    
		  boxplot(eval(parse(text=modelo3)),data=dat, cex.axis=size.font,
		    main=paste(vd,' por ',que.factor[1],':',que.factor[2],sep=''),
		    ylab=paste(vd,' dentro +-',corte.dt,' dt',sep='') )
		} # FIN SI CORTE.DT
    
		if(!is.na(identifica[1])){
		 cat('*** Presiona esc cuando termines la identificacion ***',fill=TRUE)
		 identify(as.numeric(as.factor(dat$interac)),dat[,vd])
		}	
		
 	} # FIN SI 2 FACTORES
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#
#---------------------------------------------------------------------------
 tipifica.fnc=function(datos, variable=NA){
	if(is.na(variable[1])){
		cat('',fill=TRUE)
		cat('*** Error. No has indicado que variable deseas tipificar ***',fill=TRUE)
		cat('*** Incluyela en el argumento variable.                  ***',fill=TRUE)
		cat("*** Ej. variable=5  variable='peso'                      ***",fill=TRUE)
		cat('',fill=TRUE)
		stop( )
	}
	if(length(variable) > 1){
		cat('',fill=TRUE)
		cat('*** Error. Solo puedes incluir una variable a tipificar ***',fill=TRUE)
		cat('',fill=TRUE)
		stop( )
	}
	if(is.numeric(variable)) variable=names(datos)[variable]
	pt=scale(datos[,variable])
	if(sum(is.nan(pt))==length(pt)){
		datos[,variable]=recode(datos[,variable], "Inf=NA")
		indice=is.nan(datos[,variable])
		datos$index=1:dim(datos)[1]		
		no.nan=datos[!indice,]
		si.nan=datos[indice,]
		no.nan$pt_=scale(no.nan[,variable])
		si.nan$pt_=NA
		datos=rbind(no.nan,si.nan)
		datos=ordena.por.variable.fnc(datos,'index',silente=TRUE)		
		pt=datos$pt_
	}
 	return(pt)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# dime.si.hay.factores.fnc(datos)
#---------------------------------------------------------------------------
 dime.si.hay.factores.fnc=function(datos){
	nombres=names(datos)
	check=datos
     	# Busca factores
     	es.factor=logical( )
     	for (i in 1:dim(check)[2])
                 es.factor[i]=is.factor(check[,i]) | is.character(check[,i])
	columnas=1:dim(datos)[2]
	columnas=columnas[es.factor]
	return(list(columnas=columnas, factores=nombres[es.factor]))
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# new.dat=fundir.objetos.fnc(objetos, mas.suj=FALSE, mas.var=FALSE, que.var=NA)
#---------------------------------------------------------------------------
# fundir.objetos.fnc
#---------------------------------------------------------------------------
 fundir.objetos.fnc=function(datos1=NA,datos2,mas.suj=FALSE, mas.var=FALSE, que.var=NA,
		crea.var=FALSE, todos.1=TRUE, todos.2=TRUE){

	if(class(try(is.na(datos1)))=='logical') {
		cat('',fill=TRUE)
		crea.cat.fnc('fundir.objetos.fnc')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(" #COMBINAR DOS BASES DE DATOS DE DISTINTOS SUJETOS. ADD SUJETOS  ",fill=TRUE)
		cat(" #Creamos dos bases de datos simuladas con 25 y 50 sujetos respect.  ",fill=TRUE)
		cat("  datos1=data.frame(mvrnorm(25,rep(0,10),Sigma=diag(1,10)))          ",fill=TRUE)
		cat("  datos2=data.frame(mvrnorm(50,rep(2,10),Sigma=diag(1,10)))          ",fill=TRUE)
		cat("  datos=fundir.objetos.fnc(datos1,datos2, mas.suj=T, que.var='sujeto')",fill=TRUE)
		cat(" ",fill=TRUE)
		cat(" #COMBINAR DOS BASES DE DATOS DE LOS MISMOS SUJETOS. ADD VARIABLES  ",fill=TRUE)
		cat("  datos1=data.frame(mvrnorm(25,rep(0,10),Sigma=diag(1,10)))            ",fill=TRUE)
		cat("  datos1$sujeto=1:25          								",fill=TRUE)
		cat("  datos2=data.frame(mvrnorm(25,rep(2,10),Sigma=diag(1,10)))            ",fill=TRUE)
		cat("  datos2$sujeto=1:25          								",fill=TRUE)
		cat("  datos2=cambia.nombre.var.fnc(datos2,1:10, paste('Y',1:10,sep=''))    ",fill=TRUE)
		cat("  datos=fundir.objetos.fnc(datos1,datos2, mas.var=T, que.var='sujeto')",fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Permite unir dos bases de datos, tanto para anadir variables como casos.', fill=TRUE)
		cat(" ",fill=TRUE)
		cat(" sites.google.com/site/ullrtoolbox/lectura-de-archivos/fundir-objetos-fnc", fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		print(argumentos('fundir.objetos.fnc'))
	return('help')
	}

	n.rec1=dim(datos1)[1];	 n.rec2=dim(datos2)[1]
	n.var1=dim(datos1)[2];	 n.var2=dim(datos2)[2]
	nomb1=names(datos1);	nomb2=names(datos2)
	dim.datos1=dim(datos1); dim.datos2=dim(datos2)

	if(!mas.suj & !mas.var){
		cat('',fill=TRUE)
		cat('*** Error. Debes incluir el argumento mas.var=T si vas a incluir nuevas variables ***',fill=TRUE)
		cat('*** o el argumento mas.suj=T si vas a incluir nuevas sujetos a la base de datos   ***',fill=TRUE)
		cat('',fill=TRUE)
		stop( )
	}

	# SI X.SUJETOS
	if(mas.suj){
		if(!identical(nomb1,nomb2)){
			indice1=nomb1 %in% nomb2
			indice2=nomb2 %in% nomb1
			falta2=nomb1[!indice1];	falta1=nomb2[!indice2]
			que.falta=c(falta2,falta1)
			if(!crea.var){
			    if(length(que.falta)==0){
			      cat('',fill=TRUE)
			      cat('*** Error. Los nombres de las variables no tienen el mismo orden en ambas bases de datos',fill=TRUE)
			      cat('*** Si incluyes el argumento crea.var=T se reordenara la segunda por los nombres de la primera',fill=TRUE)
			      stop( )
			    }else{
			      cat('',fill=TRUE)
			      cat('*** Error. Los nombres de las variables de ambos objetos no son iguales.',fill=TRUE)
			      cat('*** Estos nombres difieren entre ambos objetos:',que.falta,'.',fill=TRUE)
			      cat('*** Si deseas que esta variable se incluya como missign introduce el argumento crea.var=T',fill=TRUE)
			      stop( )
			    }
			}else{
			  if(length(falta1) > 0) for(i in length(falta1)) datos1[,falta1[i]]=NA
			  if(length(falta2) > 0) for(i in length(falta2)) datos2[,falta2[i]]=NA
			  nombres=names(datos1)
			  datos2=datos2[nombres]
			  crea.cat.fnc('FUNDIDO DE ARCHIVOS. ADD SUJETOS')
			  if(length(que.falta)==0){
			    cat('',fill=TRUE)
			    cat('*** WARNING. Se ha reordenado los nombres de las variables en la base de datos fundida',fill=TRUE)
			  }else{
			    cat('',fill=TRUE)
			    cat('*** WARNING. Se ha creado la variable o variables:',que.falta,'con valor NA en una de las dos bases de datos',fill=TRUE)
			  }
			  combinados=rbind(datos1,datos2)
			}
 		}else{
			crea.cat.fnc('FUNDIDO DE ARCHIVOS. ADD SUJETOS')
			combinados=rbind(datos1,datos2)
		}
	}# FIN SI X.SUJETOS

	# SE DECLARA QUE.VAR
	if(!is.na(que.var[1])){
	    ok.n1=match(que.var,nomb1);	ok.n2=match(que.var,nomb2)
	    # SI NO HAY VAR EMPAREJAMIENTO
	    if( is.na(ok.n1[1]) | is.na(ok.n2[1]) ){
		cat('',fill=TRUE)
		cat('*** Error. No existe la variable de emparejamiento en alguno de los dos objetos a combinar',fill=TRUE)
		stop( )
	    }
	}

	# SI MAS VAR
	if(mas.var){
		crea.cat.fnc('FUNDIDO DE ARCHIVOS. ADD VARIABLES')
		# SI NO SE DECLARA QUE.VAR
		if(is.na(que.var[1])) {
			cat('',fill=TRUE)
			cat('*** Error. Debes indicar la variable de emparejamiento comun a ambos archivos',fill=TRUE)
			cat('*** incluyendo el argumento que.var con el nombre de dicha variable.         ',fill=TRUE)
			stop( )
		} # FIN NO SE DECLARA QUE.VAR

		# CHECK SI FUNDIBLE
		chivato1=data.frame(with(datos1, table(datos1[,que.var]))); chivato1=chivato1[order(chivato1$Freq, decreasing=TRUE),]
		chivato2=data.frame(with(datos2, table(datos2[,que.var]))); chivato2=chivato2[order(chivato2$Freq, decreasing=TRUE),]

		indice1=nomb1 %in% nomb2
		indice2=nomb2 %in% nomb1
		repe1=nomb1[indice1];	repe2=nomb2[indice2]
		repetido=unique(c(repe1,repe2))
		if(length(repetido) > 1){
		  indice=match(que.var,repetido)
		  repetido=repetido[-indice]
		  cat('', fill=TRUE)
		  cat('*** Error. Las siguientes variables se encuentran en ambas bases de datos:',repetido,fill=TRUE)
		  cat('*** elimina la o las variables repetidas de una de las bases de datos',fill=TRUE)
		  stop( )
		}

		if(chivato1[1,2] == 1 | chivato2[1,2] == 1) {
		    combinados=merge(datos1,datos2,by=que.var,all.x=todos.1, all.y=todos.2)
		}

		if(chivato1[1,2] > 1 & chivato2[1,2] > 1){
		    if(identical(chivato1,chivato2)){
			cat('',fill=TRUE)
			cat('*** WARNING. Tu unidad de registro:',que.var,'ocupa mas de una fila en ambas bases de datos',fill=TRUE)
			cat('*** Dado que ademas ambas frecuencias por:',que.var,'son identicas en ambas, se uniran de  ',fill=TRUE)
			cat('*** forma directa',fill=TRUE)
			que.col=match(que.var, names(datos2))
			combinados=cbind(datos1,datos2[,-que.col])
		    }
		}

		if(chivato1[1,2] > 1 & chivato2[1,2] > 1){
		    if(!identical(chivato1,chivato2)){
			cat('',fill=TRUE)
			cat('*** WARNING. Tu unidad de registro:',que.var,'ocupa mas de una fila en ambas bases de datos',fill=TRUE)
			cat('*** Revisa el resultado, puede ser incorrecto.  ',fill=TRUE)
			combinados=merge(datos1,datos2,by=que.var,all.x=todos.1, all.y=todos.2)
		    }
		}

		if(chivato1[1,2] ==1  & chivato2[1,2] > 1){
		    cat('*** Eliminando registros repetidos de la segunda base de datos ***',fill=TRUE)
		    datos2=elimina.repetidos.fnc(datos2,ID=que.var)
		    combinados=merge(datos1,datos2,by=que.var,all.x=todos.1, all.y=todos.2)
		    cat('',fill=TRUE)
		    cat('*** WARNING. Tu unidad de registro:',que.var,'en la segunda base de datos ocupa mas de una fila.',fill=TRUE)
		    cat('*** Se ha eliminado en esta base de datos todas las filas repetidas de la variable',que.var,'.',fill=TRUE)
		}

		if(chivato1[1,2] > 1  & chivato2[1,2] == 1){
		    combinados=merge(datos1,datos2,by=que.var,all.x=todos.1, all.y=todos.2)
		    cat('',fill=TRUE)
		    cat('*** WARNING. Tu unidad de registro:',que.var,'en la la primera base de datos ocupa mas de una fila.',fill=TRUE)
		}
	}# FIN SI MAS VAR
	dim.combinados=dim(combinados)
	tabla=rbind(dim.datos1,dim.datos2,dim.combinados)
	colnames(tabla)=c('filas','columnas')
	cat('',fill=TRUE)
	cat('*** Dimensiones de entrada y salida ***',fill=TRUE)
	print(t(tabla))
 return(combinados)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# cambia.nombre.var.fnc(datos, antiguos=c('a','b'),nuevos=c('a1','b1'))
#---------------------------------------------------------------------------
 cambia.nombre.var.fnc=function(datos, antiguo,nuevo,silente=FALSE){
    # SI VAR ES NUMERICA
	if(is.numeric(antiguo)){
		nombres=names(datos)
		antiguo=nombres[antiguo]
	}
    # FIN SI VAR
    nombres=names(datos)
	if(length(antiguo) != length(nuevo)){
		cat('',fill=TRUE)
		cat('*** Error.La longitud de los vectores de nombres antiguos y nuevos difieren ***',fill=TRUE)
	stop( )
	}else{
		que.col=match(antiguo,nombres)
		nombres[que.col]=nuevo
		names(datos)=nombres
        if(!silente){
		    crea.cat.fnc('RENOMBRA VARIABLES')
		    cat('*** Se ha cambiado el nombre de las variables',antiguo,fill=TRUE)
		    cat('*** a los nuevos nombres:',nuevo,fill=TRUE)
		    cat('',fill=TRUE)
		    print(head(datos))
        }
	return(datos)
	}
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# p.asociada.fnc(gl=10, valor=19)
#---------------------------------------------------------------------------
 p.asociada.fnc=function(gl=10,valor=NA,confianza=NA){
	crea.cat.fnc('SIMULADOR DE PROBABILIDAD ASOCIADA')
	if(is.na(valor[1])) valor_=TRUE else valor_=FALSE
	if(is.na(valor[1])) valor=gl
	if(is.na(confianza[1])) confianza=0.95
	x=seq(0,qchisq(0.99999,gl),0.01)
  	q85=qchisq(0.85,gl);		p85=dchisq(q85,gl);
  	q95=qchisq(0.95,gl);
  	q99=qchisq(0.99,gl);
  	q999=qchisq(0.999,gl);
	plot(x,dchisq(x,gl),type='l',lwd=2,col='blue')
		mtext(paste('La distribucion Xi_2 con ',gl,'grados de libertad'))
		abline(h=0)
  		lines(c(q95,q95),c(0,p85),col='red',lwd=2)
  		lines(c(q99,q99),c(0,p85),col='red',lwd=2)
  		lines(c(q999,q999),c(0,p85),col='red',lwd=2)
      	abline(v=valor,lty=2)
        	text(q95,p85+0.05*p85,'0.95')
        	text(q99,p85+0.05*p85,'0.99')
        	text(q999,p85+0.05*p85,'0.999')
       	arrows(q999,p85+0.4*p85,0,p85+0.4*p85,length=0.15,code=3)
        	arrows(q99,p85+0.3*p85,0,p85+0.3*p85,length=0.15,code=3)
        	arrows(q95,p85+0.2*p85,0,p85+0.2*p85,length=0.15,code=3)
		points(valor,0.001,col='red',pch=19,cex=1.8)
        pc=c(qchisq(0.95,gl),qchisq(0.99,gl),qchisq(0.999,gl))
        names(pc)=c('0.95','0.99','0.999')
	acumulado=pchisq(valor,gl)
	por_arriba=1-pchisq(valor,gl)
      if (por_arriba > 0.05)  deci='NO Rechazo (p > 0.05)'
      if (por_arriba < 0.001) deci='Rechazo (p < 0.001)'
      if (por_arriba > 0.001 & por_arriba < 0.01) deci='Rechazo (p < 0.01)'
      if (por_arriba > 0.001 & por_arriba < 0.01 & confianza==0.999)
		deci='NO Rechazo (p > 0.001)'
      if (por_arriba > 0.001 & por_arriba < 0.01 & confianza==0.99)
		deci='Rechazo (p < 0.01)'
	if (por_arriba > 0.001 & por_arriba < 0.05 & por_arriba > 0.01 & confianza==0.99)
	        deci='No Rechazo (p > 0.01)'
	if (por_arriba > 0.001 & por_arriba < 0.05 & por_arriba > 0.01 & confianza==0.95)
	        deci='Rechazo (p < 0.05)'
	if (por_arriba > 0.001 & por_arriba < 0.05 & por_arriba > 0.01 & confianza 	> 0.99)
	        deci='No Rechazo (p > 0.001)'
	tabla=as.matrix(por_arriba)
	row.names(tabla)=paste('p((Chi.2)gl=',gl,')>=',valor,sep='')
	colnames(tabla)=paste('p de',valor)
	lista=list(puntos.criticos.para.las.confianzas=pc,probabilidades=tabla,
			Confianza=confianza,Decision=deci)
	cat('*** Es',valor,'una distancia cuadratica suficientemente grande    ',fill=TRUE)
	cat('*** en una distribucion ji cuadrado con',gl,'grados de libertad,',fill=TRUE)
	cat('*** como para considerarla diferente de cero con una confianza     ',fill=TRUE)
	cat('*** del',confianza*100,'% ?       OBSERVA PRIMERO LA GRAFICA.',fill=TRUE)
	cat('',fill=TRUE)
	print(lista)
	if(valor_){
	cat('------------------------------------------------------------------',fill=TRUE)
	cat('*** Prueba este otro ejemplo: p.asociada.fnc(gl=10, valor=19)   ',fill=TRUE)
	cat('*** Copia la funcion directamente desde aqui y pegala en tu scrip.',fill=TRUE)
	cat('------------------------------------------------------------------',fill=TRUE)
	}
}
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# Rutina para montar los datos de la practica conjunta con lenguaje y
# pensamiento
#---------------------------------------------------------------------------
 monta.datos.LP.fnc=function(datos,n.ensayos=6,n.item.condi=NA){
	if(is.na(n.item.condi[1]))
 	nombres=names(datos)
	selec=c("subject","trialcode","correct","latency")
	dat_=datos[,selec]
 	head(dat_,10)

	# CHECK SI SUJETOS DUPLICADOS
	frec=data.frame(frecuencias.fnc(dat_,1, silente=T)$subject$tabla)
	frec$Var1=as.character(frec$Var1)
	chiva2=frec[frec$Freq!=86,'Var1']
	if(length(chiva2!=0)){
		cat('',fill=TRUE)
		cat('*** Error. Los siguientes sujetos tienen un numero incorrecto de respuestas     ***',fill=TRUE)
		cat('*** Abre el archivo originario en LibreOffice o Excel y elimina esos sujetos    ***',fill=TRUE)
		cat('*** Guarda el archivo en formato Excel 97-2003 o texto delimitado con tabulador ***',fill=TRUE)
		cat('*** y repite el proceso de lectura y montaje de los datos.                      ***',fill=TRUE)
		cat('',fill=TRUE)
		cat('*** SUJETOS PROBLEMATICOS:',chiva2,fill=TRUE)
		cat('',fill=TRUE)
		stop( )
	}

	dat_$latency=dat_$latency*dat_$correct
	x.sujeto=split(dat_, dat_$subject)
	size=dim(x.sujeto[[1]])
	tr=matrix(NA,length(x.sujeto),size[1]-n.ensayos)
	er=matrix(NA,length(x.sujeto),size[1]-n.ensayos)
	for(i in 1:length(x.sujeto)){
		x=x.sujeto[[i]]
		x=x[-c(1:n.ensayos),]
		x=x[order(x$trialcode),]
		x.tr=x[,c(2,4)]
		x.tr$trialcode=as.character(x.tr$trialcode)
		tabla=with(x.tr, table(trialcode))
		x.er=x[,c(2,3)]
		x.er$correct=recode(x.er$correct, "NA=0")
		tr[i,]=t(x.tr[,2])
		er[i,]=t(x.er[,2])
	}
 	tr=data.frame(tr)
	er=data.frame(er)
	for(i in 1:dim(tr)[2]) tr[,i]=recode(tr[,i], "0=NA")
	size=dim(tr)
	cat('',fill=TRUE)
	cat('*** WARNING. Se asume que el numero de items de ensayos es 6  ***',fill=TRUE)
	cat('*** Si este numero fuese menor o superior a 6 indicalo con el ***',fill=TRUE)
	cat('*** argumento n.ensayos (Ej. n.ensayos=10).                   ***',fill=TRUE)
	cat('',fill=TRUE)
	cat('*** Estas son las frecuencias absolutas y el orden de los niveles',fill=TRUE)
	cat('*** de la matriz que has introducido.',fill=TRUE)
	cat('',fill=TRUE)
	print(tabla)
	cat('',fill=TRUE)
	cat('*** Se han creado dos bases de datos internas: tiempos y errores.',fill=TRUE)
	cat('*** correspondientes a',size[1],'sujetos y',size[2],'items',fill=TRUE)
	cat('',fill=TRUE)
	cat('*** IMPORTANTE',fill=TRUE)
	cat('*** Si por ejemplo has asignado la salida de esta funcion al objeto',fill=TRUE)
	cat('*** mis.datos. Los tiempos los tienes en mis.datos$tr  ',fill=TRUE)
	cat('*** y los aciertos (y errores) en  mis.datos$er  ',fill=TRUE)
	cat('',fill=TRUE)
	cat('*** Ejemplo: tiempos  = mis.datos$tr',fill=TRUE)
	cat('***          aciertos = mis.datos$er',fill=TRUE)
	cat('',fill=TRUE)
	cat('*** RECUERDA QUE DEBES SELECCIONAR PARA EL ANALISIS LOS PRIMEROS 40 ITEMS ***',fill=TRUE)
	cat('*** LOS OTROS 40 QUE DESECHAS SON PSEUDOPALABRAS                          ***',fill=TRUE)
	cat('*** Ejemplo: tiempos=tiempos[,1:40]',fill=TRUE)
	cat('',fill=TRUE)
 return(list(tr=tr, error=er))
 }
#---------------------------------------------------------------------------


#---------------------------------------------------------------------------
# divide.los.datos.fnc(datos, que.factor)
#---------------------------------------------------------------------------
 divide.por.factor.fnc=function(datos, que.factor,silente=FALSE){
	check.que.factor(que.factor)

 	x.factor=split(datos, datos[,que.factor])
 	n.x.factor=length(x.factor)
 	nombres=names(x.factor)
	if(!silente){
 	crea.cat.fnc('DIVIDE LA BASE DE DATOS POR FACTOR')
	cat('*** Se ha creado una lista con',n.x.factor,'elementos, correspondientes a los niveles del factor',que.factor,fill=TRUE)
	cat('*** Estos son los elementos de esa lista:',nombres,fill=TRUE)
	cat('*** Si deseas acceder a uno de ellos indicalo mediante: nombre.de.la.lista$que.lista',fill=TRUE)
	cat('*** donde que.lista se refiere al elemento que deseas extraer.',fill=TRUE)
	}
 return(x.factor)
 }

#---------------------------------------------------------------------------
#
#---------------------------------------------------------------------------
 extrae.muestra.fnc=function(datos=NA, que.factor=NA, n=NA, ID=NA, silente=FALSE){

	if(class(try(is.na(datos)))=='logical') {
		cat('',fill=TRUE)
		crea.cat.fnc('extrae.muestra.fnc')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat("  muestra=extrae.muestra.fnc(iris, n=60)                            ",fill=TRUE)
		cat("  muestra=extrae.muestra.fnc(iris, n=60, que.factor='Species')      ",fill=TRUE)
		cat("  muestra=extrae.muestra.fnc(datos, n=60, que.factor='grupo'        ",fill=TRUE)
		cat("  		ID='sujeto')                                             ",fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Permite extraer muestras de tamaño n de una base de datos. El usuario', fill=TRUE)
		cat(' puede solicitar que dicha muestra sea extraida a partir de los J     ', fill=TRUE)
		cat(' niveles de un factor definido por el usuario.                         ', fill=TRUE)
		cat(" ", fill=TRUE)
		cat("sites.google.com/site/ullrtoolbox/02-manipulacion-de-datos/extrae-muestra-fnc", fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		print(argumentos('extrae.muestra.fnc'))
	return('help')
	}

	if(!silente) crea.cat.fnc('EXTRACCION DE MUESTRA ALEATORIA')
	check.que.factor(que.factor)

	# CHECK SI NA n
	if(is.na(n)){
		cat('*** Error. Debes indicar al menos un valor de tamano muestral que deseas extraer',fill=TRUE)
		cat("*** Ej. muestra=extrae.muestra.fnc(mis.datos, que.factor='curso', n=20) ",fill=TRUE)
		return(datos)
	} # FIN
	if(is.na(ID)) {
		datos$ID=1:dim(datos)[1]
		ID='ID'
		if(!silente){
			cat('*** IMPORTANTE. No has introducido el argumento ID     ***',fill=TRUE)
			cat('*** Se considera que la unidad de registro es la fila  ***',fill=TRUE)
			cat('*** Si no fuera el caso incluye el argumento ID        ***',fill=TRUE)
			cat('',fill=TRUE)
		}
	}

	# CHECK SI EXISTE ID EN DATOS
	nombres=names(datos)
	que.ID=match(ID, nombres)
	if(is.na(que.ID)){
		cat('*** Error. No existe la variable que has utilizado como unidad de registro en ID',fill=TRUE)
	return(datos)
	}

	# SI NO HAY QUE.FACTOR
	if(is.na(que.factor[1])){
		todos=unique(datos[,ID])
		indice=sort(sample(length(todos),n))
		que.ID=todos[indice]
		indice=datos[,ID] %in% que.ID
		muestra=datos[indice,]
	} # FIN SI NO HAY FACTOR

	if(!is.na(que.factor[1])){
		n.fac=strsplit(que.factor,':')[[1]]
		nn.factor=length(n.fac)
		if(nn.factor==1) x.factor=split(datos, datos[,que.factor])

		if(nn.factor==2){
			datos$mi.condicion=factor(paste(datos[,n.fac[1]],datos[,n.fac[2]],sep='.'))
			x.factor=split(datos, datos$mi.condicion)
		}

		if(nn.factor > 2) cat('*** Error. El numero maximo de factores es 2 ***',fill=TRUE)

		tabla=data.frame(do.call(rbind,lapply(x.factor, function(x) length(unique(x[,ID])))))
		names(tabla)='Frecuencia'
		que.min=min(tabla$Frecuencia)

		# SI n > min(tabla)
		if(que.min < n){
			cat('*** Error. Has solicitado un n superior a la frecuencia minima del factor',que.factor,fill=TRUE)
			cat('',fill=TRUE)
			print(tabla)
		     return(datos)
		}else{
			x.factor=lapply(x.factor, function(x) {
				todos=unique(x[,ID])
				indice=sort(sample(length(todos),n))
				que.ID=todos[indice]
				indice=x[,ID] %in% que.ID
				que.muestra=x[indice,]
				return(que.muestra)})

			muestra=do.call(rbind,x.factor)
			row.names(muestra)=1:dim(muestra)[1]
		} # FIN SI N VALIDO

	nombres=names(muestra)
	hay.condi=match('mi.condicion',nombres)
	if(!is.na(hay.condi)) muestra=muestra[,-hay.condi]
	} # FIN SI QUE.FACTOR

 	if(!silente) {
		if(is.na(que.factor[1])){
			cat('*** Se ha extraido una muestra de',n,'registros',fill=TRUE)
			cat('',fill=TRUE)
		}else{
			cat('*** Se ha extraido una muestra de',n,'registros por cada nivel de',que.factor,'***',fill=TRUE)
			cat('',fill=TRUE)
		}
 	}
 return(muestra)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# FUNCION QUE ELIMINA LOS SUJETOS DUPLICADOS DE LA BASE DE DATOS
#---------------------------------------------------------------------------
 elimina.repetidos.fnc=function(datos, ID=NA){
	if(is.na(ID[1])){
		cat('',fill=TRUE)
		cat('*** Error. No has incluido el argumento ID con el nombre de la variable  ',fill=TRUE)
		cat('*** de identificacion. Debes incluir la variable a evaluar su duplicidad.',fill=TRUE)
		cat("*** datos = elimina.repetidos.fnc(datos, ID ='sujeto')                    ",fill=TRUE)
 		stop( )
	}
	nombres=names(datos)
	hay.ID=match(ID, nombres)

	if(is.na(hay.ID[1])){
		cat('',fill=TRUE)
		cat('*** Error. La variable de identificacion definida en ID no existe ***',fill=TRUE)
		stop( )
	}
	x.ID=split(datos, datos[,ID])
	x.ID=lapply(x.ID, function(x) {
		check=data.frame(table(x[,ID]))
		check=check[order(check$Freq, decreasing=TRUE),]
		if(check$Freq[1] > 1) x=x[1,]
		return(x) })
	limpios=do.call(rbind,x.ID)
	return(limpios)
 }

#---------------------------------------------------------------------------
# crea.pdf.fnc('mi_grafico',apaisado=T, silente=TRUE)
#---------------------------------------------------------------------------
 crea.pdf.fnc=function(archivo,apaisado=FALSE,silente=FALSE,ancho=7,alto=7){
	nombre=paste(archivo,'.pdf',sep='')
	width=ancho;	height=alto
   if(!silente){
	cat('***                     IMPORTANTE                            ***',fill=TRUE)
	cat('*** Has creado el archivo pdf',nombre,'como destino de       ***',fill=TRUE)
	cat('*** volcado de graficas. Una vez finalizado no podras         ***',fill=TRUE)
	cat('*** abrirlo hasta que lo cierres con la llamada a la funcion: ***',fill=TRUE)
	cat('***                        cierra.pdf.fnc( )                  ***',fill=TRUE)
   }
	if(apaisado) {
		width=12; height=10; paper='a4r'
	}else{
		width=width; height=height; paper='a4'
	}
   	pdf(file= nombre, width=width,height=height,onefile=TRUE, paper=paper)
 }
 cierra.pdf.fnc=function( )	dev.off( )
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# check.fac.inter.fnc(datos, fac.inter)
# Comprueba la existencia de los factores intergrupo declarados.
#---------------------------------------------------------------------------
 check.fac.inter=function(datos, fac.inter){
	nombres=names(datos)
	n.fac=length(fac.inter)
	hay.fac.inter=match(fac.inter,nombres)
	lista=list( )
	for(i in 1:n.fac) {
		if(is.na(hay.fac.inter[i])){
			lista[[i]]=NA
		}else{
			lista[[i]]='OK'
		}
	}
	names(lista)=fac.inter
	salida=unlist(lista)
	resul=names(salida[is.na(salida)])
	if(length(resul)!=0){
		cat('',fill=TRUE)
		cat('*** Error. El factor o factores declarados en fac.inter no existen  ',fill=TRUE)
		cat('*** en la base de datos incluida. Revisa los nombres de los factores',fill=TRUE)
		cat('*** definidos en fac.inter. Factores incorrectos:',resul,fill=TRUE)
	}
 return(resul)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#
#---------------------------------------------------------------------------
 analisis.loglineal.fnc=function(datos, variables=NA, es.tabla=FALSE, paso.a.paso=TRUE){
 	crea.cat.fnc('ANALISIS LOG-LINEAL')
	require('vcdExtra',quietly =TRUE)
	require('car',quietly =TRUE)
 	nombres=names(datos)
 	if(is.na(variables[1]) & !es.tabla) variables=names(datos)
	if(is.numeric(variables)) variables=nombres[variables]

	if(!es.tabla){
		# SON FACTORES?
		son.factores=logical( )
		for(i in 1:length(variables))
			son.factores[i]=is.factor(datos[,variables[i]])
			si.son=sum(son.factores)
		if(si.son < length(variables)){
			no.son=variables[!son.factores]
			cat('',fill=TRUE)
			stop(paste('*** Las siguientes variables no son factores',no.son,' ***',sep=''))
		}

		# CONSTRUYE MODELOS
		v=variables
		acumula=v[1];	acumula2=v[1]
		for(i in 2:length(v)){
			acumula=paste(acumula,v[i],sep='*')
			acumula2=paste(acumula2,v[i],sep=':')
		}
		modelo=paste('Freq ~ ',acumula,sep='')

		# CREA TABLAS
		tabla=frecuencias.fnc(datos,variables=acumula2,silente=TRUE,como.array=TRUE)$tabla
		tabla.out=frecuencias.fnc(datos,variables=acumula2,silente=TRUE,como.array=FALSE)$tabla
	}else{
		if(is.table(datos)) nombres=names(dimnames(datos))
		v=nombres
		acumula=v[1];	acumula2=v[1]
		for(i in 2:length(v)){
			acumula=paste(acumula,v[i],sep='*')
			acumula2=paste(acumula2,v[i],sep=':')
		}
		modelo=paste('Freq ~ ',acumula,sep='')
		tabla=datos
		tabla.out=ftable(tabla)
	}
	# ESTIMA GLM PASO A PASO
	crea.cat.fnc('Ejecutando estimacion del modelo paso a paso')
	if(paso.a.paso){
		paso.a.paso=try(step(glm(eval(parse(text=modelo)),
			data=data.frame(tabla), family=poisson),
			direction="backward",trace=0),silent=TRUE)
		seleccion=paso.a.paso$formula
		glm.mod=glm(seleccion, data=data.frame(tabla), family=poisson)
	}else{
		glm.mod=glm(modelo, data=data.frame(tabla), family=poisson)
		seleccion=modelo
	}
	# SALIDAS
	lista=list(Tabla.contingencia=tabla.out,modelo.Formula=seleccion,
		Coeficientes=summary(glm.mod), Anova=Anova(glm.mod,type=2))
	print(lista)
	mosaic(tabla,col=TRUE,shade=TRUE,main='Modelo Independiente')
	X11( )
	try(mosaic(glm.mod,shade=TRUE,col=TRUE,main='Modelo Estimado'),silent=TRUE)
	try(detach(package:vcdExtra),silent=TRUE)
	return(lista)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#
#---------------------------------------------------------------------------
 Manova.fnc=function(datos, variables, fac.inter, tipo=3, poshoc=NA, contrastes=NA){
  crea.cat.fnc('MANOVA')
  require(candisc,quietly=TRUE)
  require(car, quietly=TRUE)
  
	check.factores.fnc(fac.inter,fac.intra=NA)

  if(is.numeric(variables)){
    nombres=names(datos)
    variables=nombres[variables]
  }
  n.inter=length(fac.inter)
  if(n.inter==1) {
    efecto=fac.inter
    modelo=paste('as.matrix(datos[,variables])',' ~ ',fac.inter[1],sep='')
  }
  if(n.inter==2) {
    efecto=paste(fac.inter[1],'*',fac.inter[2],sep='')
    modelo=paste('as.matrix(datos[,variables])',' ~ ',fac.inter[1],'*',fac.inter[2],sep='')
  }
  if(n.inter==3) {
    efecto=paste(fac.inter[1],'*',fac.inter[2],'*',fac.inter[3],sep='')
    modelo=paste('as.matrix(datos[,variables])',' ~ ',
	    fac.inter[1],'*',fac.inter[2],'*',fac.inter[3],sep='')
  }

  mod.lm=lm(eval(parse(text=modelo)),data=datos)
  res.manova=Anova(mod.lm, type=tipo,test.statistic="Wilks")

  lista=list( )
  lista[['Manova']]=res.manova
  print(res.manova)

  cat('',fill=TRUE)
  cat('***      SCPC.Hipotesis  ***',fill=TRUE)
  cat('',fill=TRUE)
	print(res.manova$SSP)

  cat('',fill=TRUE)
  cat('***      SCPC.Error      ***',fill=TRUE)
  cat('',fill=TRUE)
	print(res.manova$SSPE)

 	# DISCRIMINANTE
  	n.fac=length(fac.inter)
	if(n.fac==1) efectos=fac.inter
  	if(n.fac == 2){
		int1=paste(fac.inter[1],fac.inter[2],sep=':')
		efectos=list(fac.inter[1],fac.inter[2],int1)
  	}
  	if(n.fac >= 3) {
		int1=paste(fac.inter[1],fac.inter[2],sep=':')
		int2=paste(fac.inter[1],fac.inter[3],sep=':')
		int3=paste(fac.inter[2],fac.inter[3],sep=':')
		efectos=list(fac.inter[1],fac.inter[2],fac.inter[3],
				int1,int2,int3)
  	}

	if(!is.na(poshoc[1])){
		if(is.list(efectos)){
		    que.poshoc=efectos[as.character(do.call(cbind,efectos)) %in% poshoc]
		    que.poshoc=as.character(do.call(cbind,que.poshoc))
		}else{
		    que.poshoc=poshoc
		}
		if(length(que.poshoc)==0){
			cat('',fill=TRUE)
			cat('*** Error. Revisa los contrastes poshoc introducidos      ***',fill=TRUE)
			cat('*** Si deseas estimar efectos simples y el orden de los   ***',fill=TRUE)
			cat('*** factores intergrupo fuese A y B debes indicar A:B     ***',fill=TRUE)
			cat("*** Ejemplo fac.inter=c('A','B'), poshoc='A:B'            ***",fill=TRUE)
			cat("*** Si deseas estimar un poshoc='B:A', debes indicar ese  ***",fill=TRUE)
			cat("*** mismo orden en el argumento fac.inter=c('B','A')      ***",fill=TRUE)
			cat('',fill=TRUE)
		stop( )
		}
	}

	crea.cat.fnc('Funciones Discriminantes')
	n.efec=length(efectos); las.ef=efectos[n.efec]
 	for( i in efectos){
  		mod.dis=candisc(mod.lm,type=tipo,term=i)
		puntuaciones=mod.dis$scores
		n.fun=dim(puntuaciones)[2]-length(fac.inter)
		if(!is.na(poshoc[1])) puntuaciones$sujeto=paste('suj',1:dim(puntuaciones)[1],sep='')
		nombres=list(paste('efecto',i,sep=' '),'medias',
			'coef.tipicos','coef.estructura')
  		sal=list(modelo=mod.dis,medias=mod.dis$means,
			coef.tipicos=mod.dis$coeffs.std,
			coef.estructura=mod.dis$structure)

		names(sal)=nombres
  		print(sal)
		titulo=paste('Fun.Canonicas: ',i,sep='')
		print(plot(mod.dis,main=titulo))
		if(i !=las.ef) X11( )

		# POSHOC
		if(!is.na(poshoc[1])){
			p1=que.poshoc[i==que.poshoc]
			if(length(p1)!=0){
				if(length(strsplit(p1,':')[[1]])==1){
					for(j in 1:n.fun)
						print(contrastes.poshoc.fnc(puntuaciones,vd=paste('Can',j,sep=''),
							p1,fac.inter=fac.inter,contrastes=contrastes))
				 cat('-------------------------------------------------------------------------',fill=TRUE)
				}
				if(length(strsplit(p1,':')[[1]])==2){
					for(j in 1:n.fun)
						print(efectos.simples.fnc(puntuaciones,vd=paste('Can',j,sep=''),p1,
							fac.inter=fac.inter, ylab=paste('Can',j,sep=''), apilados=TRUE))
				 cat('-------------------------------------------------------------------------',fill=TRUE)
				}
			}
		# FIN POSHOC
 		}
	}
	# FIN DISCRIMINANTE

  for(i in variables){
    modelo=paste(i,' ~ ',efecto,sep='')
    mod.lmB=lm(eval(parse(text=modelo)),data=datos)
    eso=Anova(mod.lmB,type=tipo)
    names(eso)=c('SC','gl','F','p.val')
    que.factor=efectos[[length(efectos)]]
    print(descriptivos.fnc(datos, vd=i, que.factor=que.factor))
    print(eso)
    #   lista[[paste('Univariado.',i,sep='')]]=eso
 }
 	#try(detach(package:car),silent=TRUE)
	try(detach(package:candisc),silent=TRUE)
}
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#
#---------------------------------------------------------------------------
 existe.variable.fnc=function(datos,variables){
 	nombres=names(datos)
	hay.dosp=grep(':',variables)
	if(length(hay.dosp) ==1) variables=strsplit(variables,':')[[1]]
	if(is.numeric(variables)) variables=nombres[variables]
	chivato=match(variables,nombres)
	n.NA=sum(is.na(chivato))
	if(n.NA != 0)	faltan=variables[is.na(chivato)] else faltan=NA
	lista=list(cc=n.NA,faltan=faltan)
 return(lista)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# En design de medidas repetidas apiladas, esta funcion rellena aquellos
# registros que tuvieran un numero menor al de los niveles de un determinado
# factor
# rellena.registro.fnc(datos, que.factor='medida', ID='sujeto', vd='tr')
#---------------------------------------------------------------------------
 rellena.registro.fnc=function(datos, que.factor, vd='vd', ID, elimina.ID=NA,
                                                                  silente=FALSE){
  check.que.factor(que.factor)

  check=existe.variable.fnc(datos,c(vd,que.factor,ID))
  if(!is.factor(datos[,que.factor])){
    cat('',fill=TRUE)
    cat('*** Error. La variable:',que.factor,'es de tipo caracter.           ',fill=TRUE)
    cat('*** Debes transformar la variable desde caracter a factor mediante: ',fill=TRUE)
    cat('',fill=TRUE)
    cat("*** 	Ej: datos=transforma.variable.fnc(datos, variable='mi.variable',",fill=TRUE)
    cat("***                nuevo.tipo='factor')",fill=TRUE)
    cat('',fill=TRUE)
    stop( )
  }

  if(check$cc!=0){
      cat('',fill=TRUE)
      if(check$cc==1)
	cat('*** Error. No existe la variable:',check$faltan,'en los datos introducidos ***',fill=TRUE)
      if(check$cc > 1)
	cat('*** Error. No existen las variables:',check$faltan,'en los datos introducidos ***',fill=TRUE)
      cat('',fill=TRUE)
      stop( )
    }

   # SI HAY QUE ELIMINAR REGISTROS.
   if(!is.na(elimina.ID[1])){
      indice1=datos[,ID] %in% elimina.ID
      datos=datos[!indice1,]
   }

   nlev=length(levels(datos[,que.factor]))
   f1=data.frame(table(datos[,ID]))
   names(f1)=c(ID,'frecuencia')
   out.of.limit=f1[f1$frecuencia > nlev,]

   if(dim(out.of.limit)[1] != 0){
      cat('',fill=TRUE)
      cat('*** Error. Revisa tus datos el o los siguientes',ID,'tienen mas registros de los',fill=TRUE)
      cat('*** permitidos. Introduce el argumento elimina.ID con ese numero para eliminar ',fill=TRUE)
      cat('*** dicho registro o registros de la base de datos.',fill=TRUE)
      print(out.of.limit)
      cat('',fill=TRUE)
      cat('*** 		Ej: elimina.ID=c(269,382,3456) ',fill=TRUE)
      cat('',fill=TRUE)
      stop( )
    }

   indice=f1$frecuencia!= nlev
   ID.infe=f1[indice,1]
   contador=length(ID.infe)

   # SI HAY INFERIORES
   if(contador != 0){
      indice.ID=datos[,ID] %in% ID.infe
      inferiores=datos[indice.ID,]
      superiores=datos[!indice.ID,]

      x.ID=split(inferiores, inferiores[,ID])
      # LOOP
      for(i in 1:length(x.ID)){
	x=x.ID[[i]]
	n=dim(x)[1]
	tabla=data.frame(table(x[,que.factor]) )
	fallo=as.character(tabla[tabla$Freq==0,1])

	if(length(fallo)!=0){
	  add=x[1:length(fallo),]
	  add[,que.factor]=fallo; add[,vd]=NA
	  x=rbind(x,add)
	  x=x[order(x[,que.factor]),]
	  x.ID[[i]]=x
	}
      }
      # END LOOP
      inferiores=do.call(rbind,x.ID)
      datos=rbind(inferiores,superiores)
      datos=datos[order(datos[,ID]),]
    }
    # FIN SI HAY INFERIORES

      if(!silente){
	cat('',fill=TRUE)
	cat('*** Se han rellenado:',contador,'registros ***',fill=TRUE)
	cat('',fill=TRUE)
      }
 return(datos)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# SOCIOGRAMA
#---------------------------------------------------------------------------
 sociograma.fnc=function(datos, latex=FALSE){
	require(car, quietly =TRUE)
	numero=datos$numero
	datos=datos[,-1]
	nombres=names(datos)
	chivato=match('nombre',nombres)
	crea.cat.fnc('SOCIOGRAMA')

	# CHECK SI EXISTE LA VARIABLE NOMBRE DEL SUJETO
	if(is.na(chivato)){
		cat('',fill=TRUE)
		cat('*** Error. La segunda columna de la base de datos debe llamarse nombre',fill=TRUE)
		cat('*** y debe contener en su interior el nombre de cada sujeto del sociograma',fill=TRUE)
		cat('*** dicho nombre ',fill=TRUE)

		return( )
	}
	# CARGA LA TABLA PUNTOS CRITICOS SALVOSA
 	salvosa=matrix(c(0,-1.64,-2.33,-3.09,0,1.64,2.33,3.09,0.1,-1.62,-2.55,
		-2.95,0.1,1.67,2.4,3.23,0.2,-1.59,-2.18,-2.81,0.2,1.7,2.47,3.38,
		0.3,-1.56,-2.1,-2.67,0.3,1.73,2.54,3.52,0.4,-1.52,-2.03,-2.53,0.4,
		1.75,2.62,3.67,0.5,-1.49,-1.95,-2.4,0.5,1.77,2.69,3.81,0.6,-1.46,
		-1.88,-2.27,0.6,1.8,2.76,3.96,0.7,-1.42,-1.81,-2.14,0.7,1.82,2.83,
		4.1,0.8,-1.39,-1.73	,-2.02,0.8,1.84,2.89,4.24,0.9,-1.35,-1.66,-1.9,
		0.9,1.86,2.96,4.39,1,-1.32,-1.59,-1.79	,1,1.88,3.02,4.53,1.1,-1.28,
		-1.52,-1.68,1.1,1.89,3.09,4.67),ncol=8,byrow=TRUE)
 	salvosa=data.frame(salvosa)
 	names(salvosa)=c('obli.izq','p.05','p.01','p.001','obli.der','p.05','p.01','p.001')

	# CHECK QUE A QUIEN ELIGES NO LO RECHAZAS
	x.sujetos=divide.por.factor.fnc(datos, que.factor='nombre', silente=T)
	chivato.2=lapply(x.sujetos, function(x) {
		el=x[2:9];	re=x[10:17];	xp=x[18:25];	xn=x[26:33]	
		chiva.e=sum(match(el[!is.na(el)],re[!is.na(re)]),na.rm=T)
		chiva.x=sum(match(xp[!is.na(xp)],xn[!is.na(xn)]),na.rm=T)
		if(chiva.e ==0 & chiva.x==0) resul=0 else resul=1
		return(resul) })
	error=data.frame(error=do.call(rbind,chivato.2))
	error$sujeto=row.names(error)
	row.names(error)=1:dim(error)[1]
	if(sum(error$error) != 0){
		que.suj=error[error$error != 0,2]
		cat('',fill=TRUE)
		cat('*** Error. El o los sujetos siguientes eligen a quienes rechazan ***',fill=TRUE)
		cat('*** o esperan ser elegidos y rechazados por los mismos sujetos.  ***',fill=TRUE)
		cat('*** Revisa tu base de datos para los siguientes sujetos:         ***',fill=TRUE)
		cat('',fill=TRUE)
		cat('***  ', que.suj,fill=T)
		cat('',fill=TRUE)
	stop( )
	}


	# DEFINICION FAC.INTRA
	nombres_=names(datos[,-1])
 	fac.intra=list(respuesta=c('eleccion','rechazo','exp_pos','exp_neg'))
	datos=crea.nombre.item.fnc(datos, col.empieza.item=2, n.item=32)
 	api=apila.los.datos.fnc(datos, fac.intra=fac.intra, col.empieza.item=2,n.item=32,silente=TRUE)
 	api$sujeto=factor(rep(datos$nombre,32),levels=datos$nombre)
	api=api[order(api$sujeto),]
	api$item=as.character(api$item)
	api$item=nombres_

	# RECIBIDOS
 	tabla1=frecuencias.fnc(api, variables='vd:respuesta',silente=TRUE)
 	tabla1=tabla1$tabla
 	tabla1=data.frame(tabla1)
 	names(tabla1)=c('sujeto','respuesta','vd')
 	tabla1=desapila.los.datos.fnc(tabla1,fac.intra,silente=TRUE)
 	names(tabla1)=c('NER','NRR','XPG','XNG')

	# Chek si algun sujeto no tiene valor en todo el sociograma
	tabla1$sujeto=as.numeric(row.names(tabla1))
	chivato2=match(numero, tabla1$sujeto)
	if(sum(is.na(chivato2))!=0){
		falta1=numero[is.na(chivato2)]
		vacio=tabla1[1:length(falta1),]
		vacio[,1:4]=0
		for(i in 1:length(falta1)) vacio[i,'sujeto']=falta1[i]
		tabla1=rbind(tabla1,vacio)
		tabla1=ordena.por.variable.fnc(tabla1,variable='sujeto', silente=TRUE)
		tabla1=tabla1[,1:4]
	} else{
		tabla1=tabla1[,1:4]
	}
	# Fin check si sujeto vacio	

 	row.names(tabla1)=unique(datos$nombre)
 	medias=apply(tabla1,2,mean)

	# EMITIDOS
 	x.sujeto=split(api, api$sujeto)
 	x.sujeto=lapply(x.sujeto,function(x){
 		tabla2=frecuencias.fnc(x,variables='vd:respuesta',silente=TRUE)$tabla
		return(apply(tabla2,2,sum))})

 	tabla2=data.frame(do.call(rbind,x.sujeto))
 	names(tabla2)=c('NEE','NRE','XPI','XNI')
 	sumas=apply(tabla2,2,sum)

	# ESTADISTICOS DE RESUMEN
 	p=medias/((dim(tabla1)[1])-1)
 	q=1-p
 	S=sqrt(((dim(tabla1)[1])-1)*p*q)
 	alfa=(q-p)/S
 	alfa=as.numeric(substr(alfa,1,4))
 	alfa=round(alfa,1)

 	alma.t=matrix(NA,length(alfa),2)
 	for(i in 1:length(alfa)){
		x=alfa[i]
		if(x < 0) x=0
		alma.t[i,]=abs(as.numeric(salvosa[salvosa$obli.izq == x,c(2,6)]))
 	}
 	li=medias-alma.t[,1]*S
 	ls=medias+alma.t[,2]*S
 	limites=data.frame(rbind(li,ls))
 	names(limites)=paste(names(tabla1),names(tabla2),sep='.')

 	tablas=cbind(tabla1,tabla2)
 	tablas=tablas[,c(1,5,2,6,3,7,4,8)]

 	tablas$sig.NER=0
 	try((tablas[tablas$NER < limites[1,1],]$sig.NER=1),silent=TRUE)
 	try((tablas[tablas$NER > limites[2,1],]$sig.NER=2),silent=TRUE)
 	tablas$sig.NEE=0
 	try((tablas[tablas$NEE < limites[1,1],]$sig.NEE=1),silent=TRUE)
 	try((tablas[tablas$NEE > limites[2,1],]$sig.NEE=2),silent=TRUE)
 	tablas$sig.NRR=0
 	try((tablas[tablas$NRR < limites[1,2],]$sig.NRR=1),silent=TRUE)
 	try((tablas[tablas$NRR > limites[2,2],]$sig.NRR=2),silent=TRUE)
 	tablas$sig.NRE=0
 	try((tablas[tablas$NRE < limites[1,2],]$sig.NRE=1),silent=TRUE)
 	try((tablas[tablas$NRE > limites[2,2],]$sig.NRE=2),silent=TRUE)
 	tablas$sig.XPG=0
 	try((tablas[tablas$XPG< limites[1,3],]$sig.XPG=1),silent=TRUE)
 	try((tablas[tablas$XPG> limites[2,3],]$sig.XPG=2),silent=TRUE)
 	tablas$sig.XPI=0
 	try((tablas[tablas$XPI< limites[1,3],]$sig.XPI=1),silent=TRUE)
 	try((tablas[tablas$XPI> limites[2,3],]$sig.XPI=2),silent=TRUE)
 	tablas$sig.XNG=0
 	try((tablas[tablas$XNG< limites[1,4],]$sig.XNG=1),silent=TRUE)
 	try((tablas[tablas$XNG> limites[2,4],]$sig.XNG=2),silent=TRUE)
 	tablas$sig.XNI=0
 	try((tablas[tablas$XNI< limites[1,4],]$sig.XNI=1),silent=TRUE)
 	try((tablas[tablas$XNI> limites[2,4],]$sig.XNI=2),silent=TRUE)

 	sig.apilada=stack(tablas[,9:16])
 	names(sig.apilada)=c('valor','indice')
 	sig.apilada$nombre=datos$nombre

 	x.abajo=subset(sig.apilada, valor ==1)
 	x.arriba=subset(sig.apilada, valor ==2)
 	x.abajo=split(x.abajo, x.abajo$indice)
 	x.arriba=split(x.arriba, x.arriba$indice)

 	indi.1=data.frame(indi=do.call(rbind,lapply(x.abajo, function(x) dim(x)[1])))
 	indi.2=data.frame(indi=do.call(rbind,lapply(x.arriba, function(x) dim(x)[1])))
 	abajo=x.abajo[indi.1$indi > 0]
 	arriba=x.arriba[indi.2$indi > 0]

 	abajo=lapply(abajo, function(x) as.character(x$nombre))
 	arriba=lapply(arriba, function(x) as.character(x$nombre))

 	x.sujeto=split(sig.apilada, sig.apilada$nombre)

 	significa=subset(sig.apilada, valor > 0)
 	significa$nombre=as.character(significa$nombre)

 	indices=rbind(sumas,medias,alfa,t(alma.t),as.matrix(limites))
 	indices=round(indices,3)
 	nombres=row.names(indices)
 	nombres[4]='t.inf'; nombres[5]='t.sup'
 	nombres[6]='ic.inf'; nombres[7]='ic.sup'
 	row.names(indices)=nombres
 	colnames(indices)=paste(names(tabla1),names(tabla2),sep='.')

	# GRAFICAS PIE POR SUJETO
 	to.pdf=TRUE
   	if(to.pdf){ # SI SALIDA A ARCHIVO PDF
	  name.file='graficos_sociograma.pdf'
			width=7; height=7; paper='a4'
       pdf(file= name.file ,width=width,height=height,onefile=TRUE, paper=paper)
  	}
  	par(mfrow=c(2,2))
  	indice=c('NER','NEE','NRR','NRE','XPG','XPI','XNG','XNI')
  	for(j in x.sujeto){
  		color=c('a','b','c','d','e','f','g','h')
		x=j
		for(i in 1:dim(x)[1]){
  			if(x[i,'indice']=='sig.NER' & x[i,'valor']==0) color[1]='white'
  			if(x[i,'indice']=='sig.NER' & x[i,'valor']==1) color[1]='red'
  			if(x[i,'indice']=='sig.NER' & x[i,'valor']==2) color[1]='green'

  			if(x[i,'indice']=='sig.NEE' & x[i,'valor']==0) color[2]='white'
  			if(x[i,'indice']=='sig.NEE' & x[i,'valor']==1) color[2]='red'
  			if(x[i,'indice']=='sig.NEE' & x[i,'valor']==2) color[2]='green'

  			if(x[i,'indice']=='sig.NRR' & x[i,'valor']==0) color[3]='white'
  			if(x[i,'indice']=='sig.NRR' & x[i,'valor']==1) color[3]='green'
  			if(x[i,'indice']=='sig.NRR' & x[i,'valor']==2) color[3]='red'

  			if(x[i,'indice']=='sig.NRE' & x[i,'valor']==0) color[4]='white'
  			if(x[i,'indice']=='sig.NRE' & x[i,'valor']==1) color[4]='green'
  			if(x[i,'indice']=='sig.NRE' & x[i,'valor']==2) color[4]='red'

  			if(x[i,'indice']=='sig.XPG' & x[i,'valor']==0) color[5]='white'
  			if(x[i,'indice']=='sig.XPG' & x[i,'valor']==1) color[5]='red'
  			if(x[i,'indice']=='sig.XPG' & x[i,'valor']==2) color[5]='green'

  			if(x[i,'indice']=='sig.XPI' & x[i,'valor']==0) color[6]='white'
  			if(x[i,'indice']=='sig.XPI' & x[i,'valor']==1) color[6]='red'
  			if(x[i,'indice']=='sig.XPI' & x[i,'valor']==2) color[6]='green'

  			if(x[i,'indice']=='sig.XNG' & x[i,'valor']==0) color[7]='white'
  			if(x[i,'indice']=='sig.XNG' & x[i,'valor']==1) color[7]='green'
  			if(x[i,'indice']=='sig.XNG' & x[i,'valor']==2) color[7]='red'

  			if(x[i,'indice']=='sig.XNI' & x[i,'valor']==0) color[8]='white'
  			if(x[i,'indice']=='sig.XNI' & x[i,'valor']==1) color[8]='green'
  			if(x[i,'indice']=='sig.XNI' & x[i,'valor']==2) color[8]='red'
		}
 	 pie(rep(0.12,8),labels=indice,col=color,main=unique(x$nombre),cex=0.7)
 	}
 	dev.off()

 	cat('*** Se ha guardado en el directorio activo el archivo externo:  ***',fill=TRUE)
 	cat('***       graficos_sociograma.pdf                               ***',fill=TRUE)
 	cat('',fill=TRUE)

 	ner.up=arriba$sig.NER
 	posiciones.ner.up=list(c(0,0.05),c(0,-0.05))
 	ner.dw=abajo$sig.NER
 	posiciones.ner.dow=list(c(0,0.05),c(0,-0.05))

# 	plot(0,0,pch=1,axes=FALSE,cex=60,xlab='',ylab='',main='ELECCIONES')
#		points(0,0,pch=1,cex=40)
#		points(0,0,pch=1,cex=20)

#	for(i in 1:length(ner.up))
#	text(posiciones.ner.up[[i]][1],posiciones.ner.up[[i]][2],ner.up[i],cex=0.7,col='blue')

 	api2=api
	api2$tal=character(dim(api2)[1])
 	nombre=as.character(datos$nombre)

	api2$index=1:dim(api2)[1]
	con.na=subset(api2, is.na(vd))
	sin.na=subset(api2,!is.na(vd))

     for(i in 1:length(nombre))
		sin.na[sin.na$vd==i,'tal']=nombre[i]
	api.join=rbind(sin.na,con.na)
	api.join=api.join[order(api.join$index),]
	api.join$vd=api.join$tal
	api2=api.join[,1:5]
	api2$vd=factor(as.character(api2$vd),levels=nombre)

 	api2$valor=NA
 	try((api2[!is.na(api2$vd) & api2$item=='E1',]$valor= 5),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='E2',]$valor= 4),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='E3',]$valor= 3),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='E4',]$valor= 2),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='E5',]$valor= 1),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='E6',]$valor=NA),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='E7',]$valor=NA),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='E8',]$valor=NA),silent=TRUE)

 	try((api2[!is.na(api2$vd) & api2$item=='R1',]$valor= 5),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='R2',]$valor= 4),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='R3',]$valor= 3),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='R4',]$valor= 2),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='R5',]$valor= 1),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='R6',]$valor=NA),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='R7',]$valor=NA),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='R8',]$valor=NA),silent=TRUE)

 	try((api2[!is.na(api2$vd) & api2$item=='XN1',]$valor= 5),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='XN2',]$valor= 4),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='XN3',]$valor= 3),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='XN4',]$valor= 2),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='XN5',]$valor= 1),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='XN6',]$valor=NA),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='XN7',]$valor=NA),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='XN8',]$valor=NA),silent=TRUE)

 	try((api2[!is.na(api2$vd) & api2$item=='XP1',]$valor= 5),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='XP2',]$valor= 4),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='XP3',]$valor= 3),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='XP4',]$valor= 2),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='XP5',]$valor= 1),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='XP6',]$valor=NA),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='XP7',]$valor=NA),silent=TRUE)
 	try((api2[!is.na(api2$vd) & api2$item=='XP8',]$valor=NA),silent=TRUE)

 	api3=api2;
 	nombres=names(api3)
 	nombres[1]='nombre'; nombres[6]='vd'
 	names(api3)=nombres

 	ag.elec=agrega.los.datos.fnc(subset(api3,respuesta=='eleccion'),
 		que.factor=c('sujeto','nombre'),silente=TRUE)

 	ag.rech=agrega.los.datos.fnc(subset(api3,respuesta=='rechazo'),
 		que.factor=c('sujeto','nombre'),silente=TRUE)

 	ag.expp=agrega.los.datos.fnc(subset(api3,respuesta=='exp_pos'),
 		que.factor=c('sujeto','nombre'),silente=TRUE)

 	ag.expn=agrega.los.datos.fnc(subset(api3,respuesta=='exp_neg'),
 		que.factor=c('sujeto','nombre'),silente=TRUE)


 	tabla.elec.rango=data.frame(with(ag.elec, tapply(vd, list(sujeto,nombre),mean)))
 	tabla.rech.rango=data.frame(with(ag.rech, tapply(vd, list(sujeto,nombre),mean)))
 	tabla.expp.rango=data.frame(with(ag.expp, tapply(vd, list(sujeto,nombre),mean)))
 	tabla.expn.rango=data.frame(with(ag.expn, tapply(vd, list(sujeto,nombre),mean)))

 	for(i in 1:dim(tabla.elec.rango)[2]){
		try((tabla.elec.rango[,i]=recode(tabla.elec.rango[,i],"NA='.'")),silent=TRUE)
		try((tabla.rech.rango[,i]=recode(tabla.rech.rango[,i],"NA='.'")),silent=TRUE)
		try((tabla.expp.rango[,i]=recode(tabla.expp.rango[,i],"NA='.'")),silent=TRUE)
		try((tabla.expn.rango[,i]=recode(tabla.expn.rango[,i],"NA='.'")),silent=TRUE)

 	}

 	tabla.elec=with(subset(api2,respuesta=='eleccion'), table(sujeto,vd))
 	tabla.rech=with(subset(api2,respuesta=='rechazo'), table(sujeto,vd))
 	tabla.expp=with(subset(api2,respuesta=='exp_pos'), table(sujeto,vd))
 	tabla.expn=with(subset(api2,respuesta=='exp_neg'), table(sujeto,vd))

 	elec=data.frame(with(subset(api2,respuesta=='eleccion'), table(sujeto,vd)))
 	rech=data.frame(with(subset(api2,respuesta=='rechazo'), table(sujeto,vd)))
 	expp=data.frame(with(subset(api2,respuesta=='exp_pos'), table(sujeto,vd)))
 	expn=data.frame(with(subset(api2,respuesta=='exp_neg'), table(sujeto,vd)))

 	rec.elec=sociograma.reciproco.fnc(elec)
 	rec.rech=sociograma.reciproco.fnc(rech)
 	rec.expp=sociograma.reciproco.fnc(expp)
 	rec.expn=sociograma.reciproco.fnc(expn)
 	
 	
 	rec.elec_=data.frame(do.call(rbind,strsplit(as.character(rec.elec$par),'-')))
 	rec.rech_=data.frame(do.call(rbind,strsplit(as.character(rec.rech$par),'-')))
 	
 	NRP=as.numeric(nombre); NRN=as.numeric(nombre);
 	for(i in 1:length(nombre)){
	  NRP[i]=sum(c(as.character(rec.elec_$X1),as.character(rec.elec_$X2)) %in% nombre[i])
	  NRN[i]=sum(c(as.character(rec.rech_$X1),as.character(rec.rech_$X2)) %in% nombre[i])
	  
	}
	tabla.expp.rango_=tabla.expp.rango
	tabla.elec.rango_=tabla.elec.rango
	tabla.expn.rango_=tabla.expn.rango
	tabla.rech.rango_=tabla.rech.rango	
	
	for(i in 1:dim(tabla.expp.rango)[2]){
	  tabla.expp.rango_[,i]=as.numeric(tabla.expp.rango[,i])
	  tabla.expp.rango_[,i]=recode(tabla.expp.rango_[,i], "1:1000=1")
	  tabla.elec.rango_[,i]=as.numeric(tabla.elec.rango[,i])
	  tabla.elec.rango_[,i]=recode(tabla.elec.rango_[,i], "1:1000=1")
	  
	  tabla.expn.rango_[,i]=as.numeric(tabla.expn.rango[,i])
	  tabla.expn.rango_[,i]=recode(tabla.expn.rango_[,i], "1:1000=1")
	  tabla.rech.rango_[,i]=as.numeric(tabla.rech.rango[,i])
	  tabla.rech.rango_[,i]=recode(tabla.rech.rango_[,i], "1:1000=1") 
	}
	XPIA=rep(0,length(nombre))
	for(i in 1:length(nombre)){
	  index=tabla.expp.rango_[i,] %in% 1
	  if(sum(index)!=0) XPIA[i]=sum(tabla.elec.rango_[index,i],na.rm=T)
	}
	XNIA=rep(0,length(nombre))
	for(i in 1:length(nombre)){
	  index=tabla.expn.rango_[i,] %in% 1
	  if(sum(index)!=0) XNIA[i]=sum(tabla.rech.rango_[index,i],na.rm=T)
	}
	NFX=rep(0,length(nombre));	NFX2=rep(0,length(nombre))	
	for(i in 1:length(nombre)){
	  index=tabla.expp.rango_[i,] %in% 1
	  if(sum(index)!=0) NFX[i]=sum(index)-sum(tabla.elec.rango_[index,i],na.rm=T)
	  index=tabla.expn.rango_[i,] %in% 1
	  if(sum(index)!=0) NFX2[i]=sum(index)-sum(tabla.rech.rango_[index,i],na.rm=T)  
	}
	
	NFX=NFX+NFX2
	
	NSO=rep(0,length(nombre));	NSO2=rep(0,length(nombre))
	for(i in 1:length(nombre)){
	  index=tabla.elec.rango_[i,] %in% 1
	  if(sum(index)!=0) NSO[i]=sum(tabla.rech.rango_[index,i],na.rm=T)
	  index=tabla.rech.rango_[i,] %in% 1
	  if(sum(index)!=0) NSO2[i]=sum(tabla.elec.rango_[index,i],na.rm=T)
	  
	}	
	NSO=NSO+NSO2
	
	tablas2=data.frame(NRP,NRN,XPIA,XNIA,NFX,NSO)
	row.names(tablas2)=nombre
	
  Indices=data.frame(
    Indice=c('NER','NEE','NRR','NRE','XPG','XPI','XNG','XNI'),
    Etiqueta=c('Elecciones recibidas','Elecciones emitidas',
      'Rechazos recibidos','Rechazos emitidos',
      'Expectativa positiva grupal','Expectativa positiva individual',
      'Expectativa negativa grupal','Expectativa negativa individual'))
      
  Indices2=data.frame(
    Indice=c('NRP','NRN','XPIA','XNIA','NFX','NSO'),
    Etiqueta=c('Elecciones reciprocas positivas',
      'Elecciones reciprocas negativas',
      'Expectativas positivas individuales acertadas',
      'Expectativas negativas individuales acertadas',
      'Falsas expectativas positivas o negativas',
      'Sentimientos opuestos'))      
	
 salida=list(Indices.x.sujetos=tablas[,1:8], Indices=indices,Etiquetas=Indices,
			Tabla.Salvosa=salvosa,
			Sig.inf=abajo, Sig.sup=arriba,
			Elecciones=tabla.elec.rango, Reciprocidad.elecciones=rec.elec,
			Rechazos=tabla.rech.rango, Reciprocidad.rechazo=rec.rech,
			Expectativas.eleccion=tabla.expp.rango, Reciprocidad.exp.eleccion=rec.expp,
			Expectativas.rechazo=tabla.expn.rango, Reciprocidad.exp.rechazo=rec.expn,
			Indices2.x.sujetos=tablas2,
			Etiquetas=Indices2)
 print(salida);
 #try(detach(package:car),silent=TRUE)
 if(latex) latex.fnc(salida)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#
#---------------------------------------------------------------------------
 sociograma.reciproco.fnc=function(dat){
 	dat2=subset(dat, Freq > 0)
	dat2=dat2[,1:2]

	dat2$mutua=NA
 	dat.coin=data.frame( )
 	for(i in 1:dim(dat2)[1]){
 		busca=dat2[i,1:2]
 		cam=busca[,c(2,1)]; names(cam)=c('sujeto','vd')
 		for(j in 1:dim(dat2)[1]){
			suma=sum(dat2[j,1:2]==cam)
			if(suma==2) {
				dat2[i,3]=1
				dat.coin=rbind(dat.coin,busca)
			break
			}
 		}
 	}
	if(dim(dat.coin)[1]==0){ 
		return('No hay reciprocidad')
	}else{
 		dat.coin$vd=as.character(dat.coin$vd)
 		dat.coin$sujeto=as.character(dat.coin$sujeto)
 		dat.coin=rbind(dat.coin,c('fin','fin'))

 		i=dat.coin[1,1]; j=1
 		while(i!='fin'){
			i=dat.coin[j,1]
			if(is.na(i)) break
 			busca=dat.coin[j,1:2]
 			cam=busca[,c(2,1)]; names(cam)=c('sujeto','vd')
			n.r=dim(dat.coin)[1]
 			for(l in 1:n.r){
				suma=sum(dat.coin[l,1:2]==cam)
				if(suma==2) {
					dat.coin=dat.coin[-c(l),]
				break
				}
			}
			j=j+1; 
 		}
	 	dat.coin=dat.coin[order(dat.coin$sujeto),]
		indice=dat.coin$sujeto %in% 'fin'
		if(sum(indice)!=0) dat.coin=dat.coin[!indice,]

 		names(dat.coin)=c('par1','par2')
 		row.names(dat.coin)=1:dim(dat.coin)[1]
		dat.coin$par=paste(dat.coin$par1,dat.coin$par2,sep='-')
 	return(data.frame(par=dat.coin[,3]))
	}
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# dat = p.tipica.fnc(dat, variables=variables, que.factor='famhist')
#---------------------------------------------------------------------------
# library(bestglm)
# dat=SAheart
 p.tipica.fnc=function(datos=NA, variables=NA, que.factor=NA, silente=FALSE){

	if(class(try(is.na(datos)))=='logical') {
		cat('',fill=TRUE)
		crea.cat.fnc('p.tipica.fnc')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat("  datos = p.tipica.fnc(OBrienKaiser, variables=3:17)                 ",fill=TRUE)
		cat("  datos = p.tipica.fnc(OBrienKaiser, variables=3:17,                 ",fill=TRUE)
		cat("  			que.factor='gender')                              ",fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Transforma las variables definidas por el usuario en su version en  ', fill=TRUE)
		cat(' puntuaciones tipicas (Zi). Tanto de forma general como por niveles  ', fill=TRUE)
		cat(' de un determinado factor.                                           ', fill=TRUE)
		cat(" ", fill=TRUE)
		cat(" sites.google.com/site/ullrtoolbox/02-manipulacion-de-datos/p-tipica-fnc", fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		print(argumentos('p.tipica.fnc'))
	return('help')
	}

    nombres=names(datos)
	check.que.factor(que.factor)

    datos$record=1:dim(datos)[1]
    # CHECK VARIABLES
    if(is.na(variables[1])) {
      variables=nombres
    }else{
      if(is.numeric(variables)) variables=nombres[variables]
    }

    # CHECK FACTOR Y TIPIFICACION
    if(is.na(que.factor[1])){
      f1=data.frame(datos[,variables])
	 names(f1)=variables
	 for(i in variables) f1[,i]=as.numeric(tipifica.fnc(f1,i))
      f1=data.frame(cbind(f1,datos$record))
    }else{
      check=existe.variable.fnc(datos, que.factor)
      # Existe factor?
	if(check$cc != 0) {
		cat('',fill=TRUE)
		cat('*** Error. No existe el factor',check$faltan,'en la base de datos ***',fill=TRUE)
		cat('',fill=TRUE)
		stop( )
	}
	x.factor=split(datos, datos[,que.factor])
	x.factor=lapply(x.factor, function(x) {
		x=x[,c(variables,'record')]
		f1=data.frame(x[,variables])
		names(f1)=variables
		for(i in variables) f1[,i]=as.numeric(tipifica.fnc(x,i))
	  	x=data.frame(cbind(f1,x$record))
	  	names(x)=c(variables,'record')
	  	return(x)})

      # COMBINA
      f1=data.frame(do.call(rbind,x.factor))
      f1=f1[order(f1$record),]
    } # FIN QUE.FACTOR
      names(f1)=c(paste('z.',variables,sep=''),'record')
      join=merge(datos,f1,by='record', all.x=TRUE, all.y=TRUE)
      join=join[,-1]
      if(!silente){
	crea.cat.fnc('PUNTUACIONES TIPICAS')
	cat('',fill=TRUE)
	cat('*** Se han creado las siguientes variables tipificadas: ',fill=TRUE)
	cat("***",paste('z.',variables,sep=''),fill=TRUE)
	if(!is.na(que.factor[1]))
	cat('*** en cada nivel del factor',que.factor,fill=TRUE)
	cat('',fill=TRUE)
      }
 return(join)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# A partir de dos bases de datos (origen y candidatos, la funcion
# encuentra para cada sujeto de origen los de candidatos que cumplen
# con el filtro de igualdad en las p variables cualitativas introcidas
# en el argumento variables
#---------------------------------------------------------------------------
 busca.pareja.cualitativa.fnc=function(origen,candidatos,ID,variables){
	datos1=origen;
	datos2=candidatos
	n.condi=length(variables)
	if(n.condi > 4) {
		cat('',fill=TRUE)
		cat('*** Error. El numero maximo de variables de busqueda es 4 ***',fill=TRUE)
		cat('',fill=TRUE)
		error( )
	}
	if(n.condi==1){
		datos1$condici=datos1[,variables]
		datos2$condici=datos2[,variables]
	}
	if(n.condi==2){
		datos1$condici=paste(datos1[,variables[1]],datos1[,variables[2]],sep='.')
		datos2$condici=paste(datos2[,variables[1]],datos2[,variables[2]],sep='.')
	}
	if(n.condi==3){
		datos1$condici=paste(datos1[,variables[1]],datos1[,variables[2]],
			datos1[,variables[3]],sep='.')
		datos2$condici=paste(datos2[,variables[1]],datos2[,variables[2]],
			datos2[,variables[3]],sep='.')
	}
	if(n.condi==4){
		datos1$condici=paste(datos1[,variables[1]],datos1[,variables[2]],
			datos1[,variables[3]],datos1[,variables[4]],sep='.')
		datos2$condici=paste(datos2[,variables[1]],datos2[,variables[2]],
			datos2[,variables[3]],datos2[,variables[4]],sep='.')
	}
	x.sujeto=split(datos1,datos1[,ID])
	salida=lapply(x.sujeto, function(x) {
		condi=x$condici
		indice=datos2$condici==condi
		pareja=datos2[indice,ID]
		if(length(pareja)==0) pareja=NA
		return(pareja)})
 return(salida)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# A partir de dos bases de datos (origen y candidatos, la funcion
# encuentra para cada sujeto introducido en el argunento suj.objetivo
# el candidato mas similar de los introducidos en ID.candidatos
# con el filtro de cluster en las p variables cuantitativas introcidas
# en el argumento variables. Si el argumento suj.objetivo es una lista
# la funcion genera un data.frame con los sujetos mas similares para
# cada sujeto de origen.
#---------------------------------------------------------------------------
# ID='Codigo'
# origen=inmigrantes
# candidatos=autoctonos
# ID.candidatos=parejas[[1]]
# suj.objetivo=80
 busca.pareja.cuantitativa.fnc=function(origen,candidatos,ID,suj.objetivo, ID.candidatos,
		variables, silente=FALSE){
		nombres=names(origen)
		if(is.numeric(variables)){
			variables=nombres[variables]
		}
		variables=c(ID,variables)
		col.ID=match(ID,nombres)
	datos1=origen;
	datos2=candidatos
	datos2=datos2[,variables]

	if(is.list(suj.objetivo)){
		salida=list( )
		almacen=integer( )
		for(i in 1:length(suj.objetivo)){
			print(i)
			suj.obj=as.numeric(names(suj.objetivo[i]))
			ID.candidatos=suj.objetivo[[i]]
			if(length(ID.candidatos)< 2) { salida[[i]]=ID.candidatos; next }
			if(is.na(ID.candidatos[1])) { salida[[i]]=NA; next }
			salida[[i]]=busca.pareja(datos1,datos2,ID,suj.obj, ID.candidatos,
					variables,silente)
			if(sum(salida[[i]] %in% almacen)!=0){
				cat('El par',salida[[i]],'del origen',suj.obj,'esta repetido, se intenta otro',fill=TRUE)
				salida[[i]]=busca.pareja(datos1,datos2,ID,suj.obj, ID.candidatos,
					variables,silente,repetido=TRUE)
			}
			almacen=c(almacen,salida[[i]])
		}
		names(salida)=names(suj.objetivo)
		pares=suppressWarnings(data.frame(do.call(rbind,salida)))
		pares[,ID]=row.names(pares)
		n.col=dim(pares)[2]
		pares=pares[,c(n.col,1:(n.col-1))]
		names(pares)=c(ID,paste('C',1:(n.col-1),sep=''))
	}else{
		pares=busca.pareja(datos1,datos2,ID,suj.objetivo, ID.candidatos,
					variables,silente)
	}
 crea.cat.fnc('EMPAREJAMIENTO VARIABLES CUANTITATIVAS')
 return(pares)
 }

#---------------------------------------------------------------------------
# Rutina basica de llamada para la funcion anterior
#---------------------------------------------------------------------------
 busca.pareja=function(datos1,datos2,ID,suj.obj, ID.candidatos,
	variables, silente,repetido=FALSE){
	# Selecciona de la base de datos de candidatos posibles a los definidos
	indice=datos2[,ID] %in% ID.candidatos
	candidatos=datos2[indice,variables]
	# N.VAR
	if(length(variables)== 2){
		valor=datos1[datos1[,ID]==suj.obj,variables[-1]]
		if(is.na(valor[1])){ seleccion=NA; return(seleccion)}
		candidatos$dif=abs(candidatos[,variables[-1]]-valor)
		if(dim(candidatos)[1]==0){ seleccion=NA; return(seleccion)}
		dif=min(candidatos$dif,na.rm=T)
		candidatos=candidatos[!is.na(candidatos$dif),]
		if(repetido & dim(candidatos)[1] > 1){
			indice=candidatos$dif==dif
			candidatos=candidatos[!indice,]
			dif=min(candidatos$dif)
		}
 		seleccion=candidatos[candidatos$dif==dif,variables[1]]
	}else{
		# Combina al sujeto objetivo con los candidatos
 		inicio=rbind(datos1[datos1[,ID]==suj.obj,variables],candidatos)
		matriz=as.matrix(inicio[,-1])
		rownames(matriz)=inicio[,ID]
		# Calculamos la matriz de distancia entre-sujetos
		d = dist(matriz,diag=FALSE, upper=TRUE)
		# Analisis de cluster para la matriz de distancias
		hc = hclust(d)

		# Seleccionamos al sujeto con menor distancia al sujeto objetivo
		chivato=as.matrix(d)[-1,1]
		minimo=min(chivato)
		chivato=data.frame(chivato)
		chivato$ID=row.names(chivato)
		seleccion=as.numeric(chivato[chivato$chivato==minimo,'ID'])
		if(!silente){
			plot(hc,cex=0.7, xlab='',
				main=paste('Objetivo: ',ID,' ',suj.obj,sep=''),
				sub=paste('Seleccion: ',ID,' ',seleccion,sep=''))
			box()
			orden=which(hc$order==1)
			print(abline(v=orden,lty=2,col='red'))
			x=readline('*** Presiona Intro para continuar ***' )
			if(x=='c') break()
		}
	} #CIERRE N.VAR
 	return(seleccion)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#
#---------------------------------------------------------------------------
 promedio.imputaciones.mice.fnc=function(estimado, silente=FALSE){
	if(!silente)
		crea.cat.fnc('DATOS PROMEDIO DE LAS IMPUTACIONES DEL MODELO MICE')
	if(class(estimado)!='mids'){
		cat('',fill=TRUE)
		cat('*** Error. No has introducido un modelo estimado de missing            ***',fill=TRUE)
		cat('*** Utiliza la funcion mice(mis.datos) para que se genere dicho modelo ***',fill=TRUE)
		cat('',fill=TRUE)
	stop( )
	}

	datos=complete(estimado, action=1)
	check=dime.si.hay.factores.fnc(datos)
	if(length(check$factores)!=0){
		fac_=TRUE
		elimina=check$columnas
		no.elimina=names(datos)[-elimina]
		datos=datos[,-elimina]
		names(datos)=no.elimina
		dat_=estimado$dat
	}else{
		fac_=FALSE
	}

	acumulado=datos
	for(i in 2:estimado$m){
		datos=complete(estimado, action=i)
		if(fac_){ datos=datos[,-elimina]; names(datos)=no.elimina}
		acumulado=acumulado+datos
	}
 	datos=acumulado/estimado$m

	if(!silente){
		cat('*** Se ha promediado con exito',estimado$m,'imputaciones numericas ***',fill=TRUE)
		cat('',fill=TRUE)
	}

	if(fac_) {
		dat_[,no.elimina]=datos; datos=dat_
		imputa.fac=complete(estimado, action=i)[elimina]
		dat_[,elimina]=imputa.fac
		cat('*** WARNING. Para los valores perdidos de los factores   ***',fill=TRUE)
		cat('*** se ha utilizado la ultima imputacion. NO SON MEDIAS. ***',fill=TRUE)
		datos=dat_
	}
	return(datos)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#
#---------------------------------------------------------------------------
 estima.missing.fnc=function(datos, variables=NA, metodo=NA, con.factor=FALSE){
	require(mice, quietly=TRUE)
	if(is.na(variables[1])) variables=names(datos)
	if(is.numeric(variables)) variables=names(datos)[variables]
	check.factores=dime.si.hay.factores.fnc(datos[,variables])
	if(length(check.factores$factores)!=0 & !con.factor){
		cat('',fill=TRUE)
		cat('*** WARNING. Tienes factores entre las variables seleccionadas. ***',fill=TRUE)
		cat('*** Esto impide el promedio de las n imputaciones solicitadas.  ***',fill=TRUE)
		cat('*** Elimina el factor o factores de las variables seleccionadas ***',fill=TRUE)
		cat('*** o incluye el argumento con.factor=T. Esto hara que dicho    ***',fill=TRUE)
		cat('*** factor o factores no participen en la matriz promedio final.***',fill=TRUE)
		cat('',fill=TRUE)
		cat('*** FACTORES:',check.factores$factores,fill=TRUE)
	stop( )
	}
	dat_=datos[,variables]
 	patron=md.pairs(dat_)
	names(patron)=c('completo.ambos','completo.missing',
			'missing.completo','missing.ambos')
	if(is.na(metodo[1])){
		metodo=rep('norm',length(variables))
		if(con.factor) metodo[check.factores$columnas]="logreg"
	}
	predi=quickpred(dat_,minpuc=0.1)

	if(is.na(metodo[1])){
		metodo=rep('norm',length(variables))
	}

	que.metodo=data.frame(variables,metodo)
	cat('',fill=TRUE)
	cat('*** Metodo de estimacion por variable ***',fill=TRUE)
	print(que.metodo)
	cat('',fill=TRUE)
	cat('***       Matriz de predictoras		 ***',fill=TRUE)
	print(predi)
	cat('',fill=TRUE)
	cat('***         Proceso iterativo         ***',fill=TRUE)

	estimado=mice(dat_,pred=predi, method=metodo)
	# GRAFICOS
	com=complete(estimado,'long', inc=T)
	crea.pdf.fnc('checkImputaMissing',silente=TRUE)
	for(i in 1:(dim(com)[2]-2)){
 		col=rep(c('blue','red')[1+as.numeric(is.na(estimado$data[,i]))],6)
 		que.com=cbind(com[,c(1,i+2)],col)
 		que.com[que.com$.imp==0 & que.com$col=='red','col']=NA
		grafi=stripplot(que.com[,2]~.imp, data=que.com, jit=TRUE,
			fac=0.8, col=col, pch=20, cex=1.4,
			xlab='Numero de imputacion', ylab=names(que.com)[2])
		print(grafi)
 		if(i==(dim(com)[2]-2)) break( )
 	}
 	cierra.pdf.fnc( )

	cat('',fill=TRUE)
	cat('*** Este es el patron de missing de la base de datos estimada ***',fill=TRUE)
	cat('',fill=TRUE)
	print(patron)
	cat('',fill=TRUE)
	cat('*** Se ha guardado el archivo checkImputaMissing.pdf en la carpeta activa ***',fill=TRUE)
	cat("*** Puedes probar este otro metodo de estimacion: 'pmm'                   ***",fill=TRUE)
	cat("*** Ej.  metodo=c('','norm','pmm','norm')                                 ***",fill=TRUE)
	cat('',fill=TRUE)
	datos.pr=promedio.imputaciones.mice.fnc(estimado)
 	datos[,variables]=datos.pr
 	try(detach(package:mice),silent=TRUE)
 return(datos)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# Comprueba que no se use las palabras condi, condicion o factor
# en la declaracion de nombres de factores inter intra o niveles.
#---------------------------------------------------------------------------
 check.factores.fnc=function(fac.inter=NA, fac.intra=NA){
 	reservadas=c('condi','condicion','factor')
	if(!is.na(fac.inter[1])){
		hay.inter=reservadas %in% fac.inter
		cual.inter=reservadas[hay.inter]
		chivato.inter=sum(hay.inter,na.rm=T)
	}else{
		chivato.inter=0
	}
	if(!is.na(fac.intra[1])){
		hay.intra=reservadas %in% names(fac.intra)
		x.niveles=lapply(fac.intra, function(x) reservadas %in% x)
		x.niveles=data.frame(do.call(rbind,x.niveles))
		names(x.niveles)=reservadas
		chivato.niveles=sum(apply(x.niveles,2,sum))
		cual.intra=reservadas[hay.intra]
		chivato.intra=sum(hay.intra)
	}else{
		chivato.intra=0
		chivato.niveles=0
	}
	if(chivato.inter!=0){
		cat('',fill=TRUE)
		cat('*** Error. Los nombres de variables:',reservadas,'son reservados. ***',fill=TRUE)
		cat('*** Modifica en tu base de datos el nombre de la variable:',cual.inter,fill=TRUE)
		cat("*** Ej:  datos=cambia.nombre.var(datos,'condicion','condici')",fill=TRUE)
		cat('',fill=TRUE)
	stop( )
	}
	if(chivato.intra!=0){
		cat('',fill=TRUE)
		cat('*** Error. Los nombres:',reservadas,'son reservados.         ***',fill=TRUE)
		cat('*** Modifica el nombre de los factores del objeto fac.intra ***',fill=TRUE)
		cat('',fill=TRUE)
	stop( )
	}
	if(chivato.niveles!=0){
		cat('',fill=TRUE)
		cat('*** Error. Los nombres:',reservadas,'son reservados.         ***',fill=TRUE)
		cat('*** Modifica el nombre de los niveles del objeto fac.intra  ***',fill=TRUE)
		cat('',fill=TRUE)
	stop( )
	}
	# CHECK SI FAC.INTRA CONTIENE EL CARACTER PUNTO
	if(!is.na(fac.intra[1])){
		for(j in 1:length(fac.intra)) {
			x=fac.intra[[j]]
			for(i in 1:length(x)){
				chivato1=strsplit(as.character(x[i]),"[:.:]")[[1]]
				chivato2=strsplit(as.character(x[i]),"-")[[1]]
				chivato3=strsplit(as.character(x[i]),"[:+:]")[[1]]
				chivato4=strsplit(as.character(x[i]),"[:*:]")[[1]]
				if(length(chivato1) !=1 | length(chivato2) !=1 | length(chivato3) !=1
					| length(chivato4) !=1){
					cat('',fill=TRUE)
					cat('*** Error. Los niveles de los factores no pueden contener el    ***',fill=TRUE)
					cat('*** caracter punto o menos (-), +, *, etc. Puedes utilizar      ***',fill=TRUE)
					cat('*** por ejemplo el caracter (_).	                                ***',fill=TRUE)
					cat('',fill=TRUE)
					cat("*** Ej. fac.intra=list(A=c('a_low','a_hig')) o                  ***",fill=TRUE)
					cat("*** Ej. fac.intra=list(A=c('aLow','aHig'))                      ***",fill=TRUE)
					cat('',fill=TRUE)
					cat('   Este este es el contenido de tu fac.intra. Modificalo. ',fill=TRUE)
					print(fac.intra)
					stop( )
				}
			}
		}
	}
	# FIN CHECK SI PUNTO
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# Crea nuevas variables a partir de la media, mediana o suma de las
# variables incluidas obligatoriamente en una lista
#
# datos=compute.fnc(datos, variables=list(depresion=c(2,7,14,21,19,58),
#                                         ansiedad=c(1,5,13,18))
#---------------------------------------------------------------------------
 compute.fnc=function(datos=NA,variables=NA, expresion=NA, estadistico='media'){

	if(class(try(is.na(datos)))=='logical') {
		cat('',fill=TRUE)
		crea.cat.fnc('compute.fnc')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat("  mi.lista=list( F1=c(1,5,10,2,8,3), F3=c(9,11,7) )                  ",fill=TRUE)
		cat("  datos= compute.fnc(iqitems, variables=mi.lista)                    ",fill=TRUE)
		cat("  datos= compute.fnc(iqitems, variables=mi.lista, estadistico='suma')",fill=TRUE)
		cat("  datos= compute.fnc(iqitems, variables=mi.lista, estadistico='mediana')",fill=TRUE)
		cat("  datos= compute.fnc(iqitems, variables=mi.lista, estadistico='sc')",fill=TRUE)
		cat('',fill=TRUE)
		cat("  mi.expresion=list(t1=c('1.34*iq1+0.33*iq44'),t2=c('log(iq20)+2.4*iq2))",fill=TRUE)
		cat("  datos= compute.fnc(iqitems, expresion=mi.expresion)"                   ,fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Crea nuevas variables a partir de la aplicacion de una funcion de resumen', fill=TRUE)
		cat(' (media, mediana, suma, sc (suma de cuadrados) a las variables de la base ', fill=TRUE)
		cat(' de datos definidas por el usuario). Si se omite el argumento estadistico,', fill=TRUE)
		cat(' la funcion utilizara por defecto la media.                               ', fill=TRUE)
		cat(" ", fill=TRUE)
		cat("sites.google.com/site/ullrtoolbox/02-manipulacion-de-datos/nueva-variable", fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		print(argumentos('compute.fnc'))
	return('help')
	}
  
  if(is.na(variables[1]) & is.na(expresion[1])){
    cat('',fill=TRUE)
    cat('*** Error. Debes utilizar o el argumento variables o el argumento expresion',fill=TRUE)  
    cat('*** Ej. variables=list(F1=c(1,2,6,8), F2=10:20, F3=c(21:25,33)) ',fill=TRUE) 
    cat("*** Ej. expresion=list(nueva.variable='1.8*V1+0.77*V3+0.25*V6') ",fill=TRUE) 
    stop()
  }
   
  hay.expresion=FALSE; hay.variables=FALSE
	if(!is.na(variables[1]) & !is.na(expresion[1])){
	  cat('',fill=TRUE)
	  cat('*** Error. No puedes utilizar a la vez los argumentos variables y expresion.',fill=TRUE)  
	  cat('*** Si eliges variables. Deseas generar nuevas variables que son media, suma,',fill=TRUE) 
	  cat("*** mediana o suma de cuadrados de otras. Ej. variables=list(A=c('v1',v2','v3') ",fill=TRUE) 
	  cat('*** Si eliges expresion. Deseas crear nuevas variables que obedecen al algoritmo' ,fill=TRUE) 
	  cat("*** indicado en el cuerpo de la expresion. Ej: expresion=list(v8='v1^2-log(v2)')",fill=TRUE) 
  	cat('',fill=TRUE)    
	  stop()
	}
  
	if(is.na(expresion[1]) & !is.list(variables)){
		cat('',fill=TRUE)
		cat('*** Error. El argumento variables debe ser una lista con al menos un elemento ***',fill=TRUE)
		cat('*** con el nombre que deseas para la nueva variable y los nombres o numero de ***',fill=TRUE)
		cat('*** las variables que entran en la operacion:                                 ***',fill=TRUE)
		cat('*** Ej: variables=list(depresion=c(2,7,14,21,19,58), ansiedad=c(1,5,13,18))   ***',fill=TRUE)
		cat('',fill=TRUE)
	stop( )
	}
  
  if(is.na(variables[1]) & !is.list(expresion)){
		cat('',fill=TRUE)
		cat('*** Error. El argumento expresion debe ser una lista con al menos un elemento ***',fill=TRUE)
		cat('*** con el nombre que deseas para la nueva variable y el algoritmo que deseas ***',fill=TRUE)
		cat('*** aplicar a las variables en cuestion.                                      ***',fill=TRUE)
		cat("*** Ej: expresion=list(pred1='1.6*v1+2.4*v2+0.77*v5')                         ***",fill=TRUE)
		cat('',fill=TRUE)
	stop( )
  }    
   
  if(!is.na(variables[1])){
		n.var=length(variables)
		new.var=names(variables)
    hay.variables=TRUE
	}
  if(!is.na(expresion[1])){
  	n.var=length(expresion)
		new.var=names(expresion)
    hay.expresion=TRUE
	}
  # SI HAY VARIABLES Y NO EXPRESION
  if(hay.variables){
     
    if(estadistico!='media' & estadistico!='suma' & estadistico!='mediana' & estadistico!='sc'){
		  cat('',fill=TRUE)
		  cat('*** Error. El argumento estadistico solo puede ser media, mediana o suma ***',fill=TRUE)
		  cat('',fill=TRUE)
	  stop( )
	 }

	  nombres=names(datos)
	  var.total=length(datos)
	  lista2=list( )
	  for(i in 1:n.var){
		  vari=variables[[i]]
		  if(is.numeric(vari)){
			  if(max(vari) > var.total){
				  cat('',fill=TRUE)
				  cat('*** Error. Has indicado en la lista',i,'un numero de variable fuera del rango ***',fill=TRUE)
				  cat('*** de variables disponibles                                                  ***',fill=TRUE)
				  cat('',fill=TRUE)
				  print(variables)
				  stop( )
			  }else{
				  vari=nombres[vari]
				  lista2[[i]]=vari
			  }
		  }
		  indice=apply(is.na(datos[,vari]),1,sum)==length(vari)
		  if(estadistico=='media')
			  datos[,new.var[i]]=apply(datos[,vari],1,function(x) mean(x,na.rm=TRUE))
		  if(estadistico=='mediana')
			  datos[,new.var[i]]=apply(datos[,vari],1,function(x) median(x,na.rm=TRUE))
		  if(estadistico=='suma')
			  datos[,new.var[i]]=apply(datos[,vari],1,function(x) sum(x,na.rm=TRUE))
		  if(estadistico=='sc')
			  datos[,new.var[i]]=apply(datos[,vari],1,function(x) sum(x^2, na.rm=T))
		  datos[indice,new.var[i]]=NA
	  }
	  if(length(lista2)!=0) names(lista2)=names(variables)
	    cat('',fill=TRUE)
	  if(n.var==1)
	    cat('*** Se ha creado la nueva variable',new.var,'aplicando la funcion de resumen',estadistico,fill=TRUE)
	  if(n.var > 1)
	    cat('*** Se han creado las nuevas variables',new.var,'aplicando la funcion de resumen',estadistico,'***',fill=TRUE)
	    cat('*** siguiendo el siguiente criterio:',fill=TRUE)
	    cat('',fill=TRUE)
	  if(length(lista2)!=0){
		  print(lista2)
	  }else{
		  print(variables)
	  }
	  cat('',fill=TRUE)
  }# FIN SI VARIABLES

  # SI HAY VARIABLES Y NO EXPRESION
  if(hay.expresion){  
    for(i in 1:n.var) datos[,new.var[i]]=with(datos, eval(parse(text=expresion[[i]])))
    if(n.var==1){
      cat('',fill=TRUE)      
	    cat('*** Se ha creado la nueva variable',new.var,'aplicando la siguiente expresion',fill=TRUE)
      print(expresion)
    }
	  if(n.var > 1){
      cat('',fill=TRUE)      
	    cat('*** Se han creado las nuevas variables',new.var,'aplicando las siguientes expresiones',fill=TRUE) 
      print(expresion)
    }
  } # FIN SI EXPRESION
    
 return(datos)
 }
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#
#---------------------------------------------------------------------------
 check.que.factor=function(que.factor){
	if(is.numeric(que.factor)){
		cat('',fill=TRUE)
		cat('*** Error. El argumento que.factor NUNCA puede ser numerico ***',fill=TRUE)
		cat('*** Sustituye el numero de columna por el nombre del factor ***',fill=TRUE)
		cat("*** en el argumento que.factor. Ej. que.factor='zona'       ***",fill=TRUE)
		cat('',fill=TRUE)
		cat('*** que.factor=',que.factor,fill=TRUE)
		stop( )
 	}
 }	
#---------------------------------------------------------------------------
# Estima el modelo de Rasch con 1, 2 y 3 parametros
# 1 parametro tri.fnc(LSAT)
# 2 parametro tri.fnc(LSAT,tipo=2)
# 3 parametro tri.fnc(LSAT,tipo=3)
#---------------------------------------------------------------------------
 tri.fnc=function(datos=NA, variables=NA, tipo=1, guarda.pf=FALSE, unidim=FALSE, 
	par.file=FALSE, grafica=TRUE, to.pdf=FALSE, nombre=NA, residuales=FALSE, apaisado=FALSE) {
	if(class(try(is.na(datos)))=='logical') {
		cat('',fill=TRUE)
		crea.cat.fnc('ANALISIS TRI')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat("  # Modelo de Rasch de 1 parametro (Dificultad)                         ",fill=TRUE)
		cat("    tri.fnc(LSAT)                                                       ",fill=TRUE)
		cat("  # Guarda archivo con los parametros estimados                         ",fill=TRUE)
		cat("    tri.fnc(LSAT, par.file=T)                                           ",fill=TRUE)
		cat("  # Contrasta la Ho de que solo hay una dimension (por defecto F)       ",fill=TRUE)
		cat("    tri.fnc(LSAT, unidim=T)                                             ",fill=TRUE)
		cat("  # Modelo TRI 2 parametros (Disciminacion y Dificultad)                ",fill=TRUE)
		cat("    tri.fnc(LSAT, tipo=2)                                               ",fill=TRUE)
		cat("  # Modelo TRI 3 parametros (Guessing, Disciminacion y Dificultad)      ",fill=TRUE)
		cat("    tri.fnc(LSAT, tipo=3)                                               ",fill=TRUE)
		cat("  # Guardamos la variable latente Habilidad por sujeto                  ",fill=TRUE)
		cat("    datos= tri.fnc(LSAT, tipo=3, guarda.pf= T)                          ",fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Estima el modelo de teoria de respuesta al item, segun el modelo de    ', fill=TRUE)
		cat(' Rasch (tipo=1), 2 parametros (tipo=2) o 3 parametros (tipo=3)          ', fill=TRUE)
		cat(' Con el argumento guarda.pf=T se guardan las puntuaciones de la variable', fill=TRUE)
		cat(' latente del modelo TRI (nivel de habilidad) para cada sujeto            ', fill=TRUE)
		cat(" ", fill=TRUE)
		cat("             https://sites.google.com/site/ullrtoolbox	              ", fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		print(argumentos('tri.fnc'))
	return('help')
	}

	if(tipo==1 & residuales) residuales=FALSE
	suppressWarnings(require(ltm, quietly=TRUE))
	nombres=names(datos)
	datos$ID=1:dim(datos)[1]
	if(is.na(variables[1])) variables=nombres
	if(is.numeric(variables[1])) variables=nombres[variables]
	dat_=datos[,variables]
	desc=descript(dat_)
	crea.cat.fnc('ANALISIS TRI')

	# PARA MAS DE 6 ITEMS CREARA GRAFICOS DE 5 EN 5 ITEMS
	n.item=length(variables)
	if(n.item <=6) {
		item_=NULL; corte=0
		lista.secu=list(1:n.item)
	}else{
		corte=ceiling(n.item/6)
		secu=seq(1:n.item)
		lista.secu=list( )
		origen=1; fin=6
		for(i in 1:corte){ 
			lista.secu[[i]]=secu[origen:fin]
			resto=length(secu)-fin
			if(resto >= 6){
		   	  origen=fin+1; fin=origen+5
			}else{
		   	  origen=fin+1; fin=origen+(resto-1)
			}
		}
	}
	# FIN DE SECUENCIA GRAFICA DE LOS ITEMS

	if(tipo==1){
		mod.rasch=rasch(dat_, constraint = cbind(length(dat_) + 1, 1))
		mod.rasch.alfa=rasch(dat_)
		res1=summary(mod.rasch)
		if(unidim){
		  cat('',fill=TRUE)
		  cat('***  Comprobando unidimensionalidad del modelo de Rasch ***',fill=TRUE)
		  cat('***  Se paciente. Es un proceso iterativo.              ***',fill=TRUE)
		  cat('',fill=TRUE)
		  unidim=unidimTest(mod.rasch)
		}else{
		  unidim='No se ha calculado la unidimensionalidad de la matriz'
		}

		res2=summary(mod.rasch.alfa)
		orden1=coef(mod.rasch, prob = TRUE, order = TRUE)
		orden2=coef(mod.rasch.alfa, prob = TRUE, order = TRUE)
		cat('',fill=TRUE)
		cat('*** Comprobando el ajuste mediante extraccion bootstrap ***',fill=TRUE)
		cat('***  Se paciente. Se extraen 200 muestras.              ***',fill=TRUE)
		cat('',fill=TRUE)
		boot1=GoF.rasch(mod.rasch, B = 199)
		boot2=GoF.rasch(mod.rasch.alfa, B = 199)
		res.anova=anova(mod.rasch,mod.rasch.alfa)
		if(unlist(res.anova$p.value) <= 0.05) result=mod.rasch.alfa else result=mod.rasch
		salida=list(modelo='Modelo de rasch (1 parametro: Dificultad)',descriptivos=desc, 
				Unidimensionalidad=unidim, modelo.rash=res1,
				modelo.estima.alfa=res2, anova=res.anova)
		modelo=mod.rasch
		print(salida)

		if(par.file){
			nombre=paste('tri_1_parametro_',Sys.time( ),sep='')
			exporta.txt.fnc(summary(result)$coefficients, nombre, nombre.fila=TRUE)
		}
		if(guarda.pf | residuales) {
			cat('',fill=TRUE)
			cat('***              CALCULANDO PUNTUACIONES FACTORIALES               ***',fill=TRUE)
			cat('',fill=TRUE)
			pred=factor.scores(result, resp.patterns = dat_)$score.dat
		}
		if(guarda.pf){
			pred=factor.scores(result, resp.patterns = dat_)$score.dat
			X11(); print(multi.hist(pred$z1, 
				main='Histograma, Densidad y Ajuste a la normal'))
			pred$ID=1:dim(pred)[1]
			variables=na.omit(match(names(datos),variables))
			datos_=data.frame(datos[,-c(variables)])
			if(dim(datos_)[2]==1) names(datos_)='ID'
			fundido=fundir.objetos.fnc(datos_,pred, 
				mas.var=TRUE, que.var='ID')
			fundido=fundido[,-1]
		}
	}
	if(tipo==2){
		mod.rasch.alfa=rasch(dat_)
		mod.2p=ltm(dat_ ~ z1)
		modelo=mod.2p
		if(unidim){
		  cat('',fill=TRUE)
		  cat('***  Comprobando unidimensionalidad del modelo de Rasch ***',fill=TRUE)
		  cat('***  Se paciente. Es un proceso iterativo.              ***',fill=TRUE)
		  cat('',fill=TRUE)
		  unidim=unidimTest(mod.2p)
		}else{
		  unidim='No se ha calculado la unidimensionalidad de la matriz'
		}
		res.anova=anova(mod.rasch.alfa, mod.2p)
		salida=list(modelo='Modelo (2 parametros: Discriminacion - Dificultad)',descriptivos=desc, 
				Unidimensionalidad=unidim, modelo.rash=summary(mod.rasch.alfa),
				modelo.2p=summary(mod.2p), anova=res.anova)
		modelo=mod.2p
		print(salida)

		if(par.file){
			nombre=paste('tri_2_parametros_',Sys.time( ),sep='')
			print(exporta.txt.fnc(summary(mod.2p)$coefficients, nombre, nombre.fila=TRUE))
		}

		if(guarda.pf | residuales){
			cat('',fill=TRUE)
			cat('***              CALCULANDO PUNTUACIONES FACTORIALES               ***',fill=TRUE)
			cat('',fill=TRUE)
			pred=factor.scores(mod.2p, resp.patterns = dat_)$score.dat
		}
		if(guarda.pf){
			X11(); print(multi.hist(pred$z1, 
				main='Histograma, Densidad y Ajuste a la normal'))
			pred$ID=1:dim(pred)[1]
			variables=na.omit(match(names(datos),variables))
			datos_=data.frame(datos[,-c(variables)])
			if(dim(datos_)[2]==1) names(datos_)='ID'
			fundido=fundir.objetos.fnc(datos_,pred, 
				mas.var=TRUE, que.var='ID')
			fundido=fundido[,-1]
		}
	}

	if(tipo==3){
		mod.rasch.alfa=rasch(dat_)
		mod.2p=ltm(dat_ ~ z1)
		mod.3p=try(tpm(dat_, IRT.param = TRUE), silent=T)
		modelo=mod.3p
		res.mod.3p=try(summary(mod.3p), silent=T)
		if(class(res.mod.3p)=='try-error') {
			res.mod.3p='*** SOLUCION NO FIABLE ***'
			cat('*** Resultados NO FIABLES de la estimacion ***',fill=TRUE)
			print(mod.3p)
		}
		if(unidim){
		  cat('',fill=TRUE)
		  cat('***  Comprobando unidimensionalidad del modelo de Rasch ***',fill=TRUE)
		  cat('***  Se paciente. Es un proceso iterativo.              ***',fill=TRUE)
		  cat('',fill=TRUE)
		  unidim=unidimTest(mod.3p)
		}else{
		  unidim='No se ha calculado la unidimensionalidad de la matriz'
		}
		res.anova=anova(mod.rasch.alfa, mod.3p)

		salida=list(modelo='Modelo (3 parametros: Guessing - Discriminacion - Dificultad)',descriptivos=desc, 
				Unidimensionalidad=unidim, modelo.rash=summary(mod.rasch.alfa),
				modelo.3p=res.mod.3p, anova=res.anova)
		modelo=mod.3p
		print(salida)

		if(par.file){
			nombre=paste('tri_3_parametros_',Sys.time( ),sep='')
			exporta.txt.fnc(summary(mod.3p)$coefficients, nombre, nombre.fila=TRUE)
		}
		if(guarda.pf | residuales){
			cat('',fill=TRUE)
			cat('***              CALCULANDO PUNTUACIONES FACTORIALES               ***',fill=TRUE)
			cat('',fill=TRUE)
 			pred=factor.scores(mod.3p, resp.patterns = dat_)$score.dat
		}
		if(guarda.pf){
			X11(); print(multi.hist(pred$z1, 
				main='Histograma, Densidad y Ajuste a la normal'))
			pred$ID=1:dim(pred)[1]
			variables=na.omit(match(names(datos),variables))
			datos_=data.frame(datos[,-c(variables)])
			if(dim(datos_)[2]==1) names(datos_)='ID'
			fundido=fundir.objetos.fnc(datos_,pred, 
				mas.var=TRUE, que.var='ID')
			fundido=fundido[,-1]
		}
	}

	if(grafica){
		if(!to.pdf){
		  for(i in 1:length(lista.secu)) {
		  item_=lista.secu[[i]]; ini=min(item_); fin=max(item_)
		  plot(modelo, type='ICC',xlab='Habilidad', ylab='Prob.',item=item_,
			main=paste('Curva caracteristica de los items: ',ini,' a ',fin,sep=''))
		  x11( )
		  }
		  for(i in 1:length(lista.secu)) {
		    item_=lista.secu[[i]]; ini=min(item_); fin=max(item_)
		    plot(modelo, type='IIC',xlab='Habilidad', ylab='Prob.',item=item_,
			main=paste('Curva de informacion de los items: ',ini,' a ',fin,sep=''))
		 
		    if(i != length(lista.secu)) x11( )
		  }
		}else{
		  if(is.na(nombre[1])){
		      nombre.file='graficas_TRI.pdf'
	  	      name.file=nombre.file
		  }else{
		      nombre.file=paste('graficas_TRI_',nombre,'.pdf',sep='')
		      name.file=nombre.file
		  }
		  pdf(file= name.file ,width=7,height=7,onefile=TRUE, paper='a4')
		  for(i in 1:length(lista.secu)) {
		  item_=lista.secu[[i]]; ini=min(item_); fin=max(item_)
		  plot(modelo, type='ICC',xlab='Habilidad', ylab='Prob.',item=item_,
			main=paste('Curva caracteristica de los items: ',ini,' a ',fin,sep=''))

		  }
		  for(i in 1:length(lista.secu)) {
		    item_=lista.secu[[i]]; ini=min(item_); fin=max(item_)
		    plot(modelo, type='IIC',xlab='Habilidad', ylab='Prob.',item=item_,
			main=paste('Curva de informacion de los items: ',ini,' a ',fin,sep=''))
		  }
        	  dev.off()
              cat('',fill=TRUE)
        	  cat('*** Se ha creado el archivo pdf',nombre.file,'en el directorio activo    ***',fill=TRUE)
		}
	}
 
 if(residuales) {
	cat('',fill=TRUE)
	cat('***                   ANALIZANDO LOS RESIDUALES (Prieto, P. 2012)                  ***',fill=TRUE)
	cat('',fill=TRUE)
	residuales.tri.fnc(modelo=modelo, tipo=tipo, pred=pred, lista.secu=lista.secu, 
		to.pdf=to.pdf, apaisado=apaisado, nombre=nombre)
 }

 if(guarda.pf){
	cat('',fill=TRUE)
	cat('*** Se han guardado las puntuaciones predichas de habilidad por sujeto ***',fill=TRUE)
	return(fundido)
 }
 try(detach(package:ltm),silent=TRUE)
 }
#---------------------------------------------------------------------------

# -------------------------------------------------------------------------------
# Codigo realizado por Dr. Prieto, P. (2012)
# -------------------------------------------------------------------------------
 residuales.tri.fnc=function(modelo, tipo, pred, lista.secu, to.pdf=TRUE, apaisado=FALSE, 
	                                                                nombre=nombre){
	#Divide Zeta en 10 intervalos
	categorias=10
	npar=tipo
	nsuj=dim(pred)[1]
	nitem=dim(pred)[2] - 4; nsuj=length(pred$z1)
	sumobs1=apply(pred[,1:nitem],1,function(x) sum(x,na.rm=TRUE))
	param <-summary(modelo)$coefficients
	param=matrix(param[,1],ncol=tipo)

	if(tipo==1) { a=param[,1]; b=0; c=0 }
	if(tipo==2) { b <-param[,1]; a <-param[,2]}
 	if(tipo==3) { c=param[,1]; b=param[,2]; a=param[,3]}

	pred$niveles <- cut(pred$z1,categorias, labels=FALSE)
    
	lista=list( );	listaji=list();	lista.sim=list( );	listaji.sim=list()
	variables=names(pred)

	mediat=agrega.los.datos.fnc(pred, vd=variables[nitem+3],
 		que.factor='niveles',estadistico='media', silente=T)
 	n=agrega.los.datos.fnc(pred, vd=variables[1],
 		que.factor='niveles',estadistico='n', silente=T)
 
 	xx <- cbind(n,mediat[,2])
 	names(xx)=c('Categoria','N', 'Z_media')
 	#print (xx)

 	for(i in 1:nitem){
		if(tipo==1){
			p=1/(1+exp(-a[i]*(mediat[2])))
		}
		if(tipo==2){
			p=1/(1+exp(-a[i]*(mediat[2]-b[i])))
		}
		if(tipo==3){
			p=c[i]+ ((1-c[i])/(1+exp(-a[i]*(mediat[2]-b[i]))))
		}
		media=agrega.los.datos.fnc(pred, vd=variables[i],
 			que.factor='niveles',estadistico='media', silente=T)
		residuos=(media[,2]-p)/ sqrt(p*(1-p)/n[,2])
		ertip= sqrt(p*(1-p)/n[,2])
		xires=n[,2]*(media[,2]-p)^ 2/( p*(1-p))
		ji2=sum(xires)
		gl=categorias-npar
		prob= pchisq(c(ji2), df=gl, lower.tail=FALSE)
		Item=cbind(i,mediat,n[,2],media[,2],p, ertip,residuos, xires) 
		names(Item) =c('item','nivel','Z','n' ,'po', 'pt', 'er_tip', 'r_stand','chi2')
		Itemji= data.frame(cbind(i,ji2, gl, prob))
		names(Itemji)= c('item','chi2','gl','p > chi2')
		lista[[i]]=Item 
		listaji[[i]]=Itemji
 	}

	todos=do.call(rbind,lista)
	todos$ID=row.names(todos)
	#print(todos)
	todosji=do.call(rbind,listaji)

	#SIMULACIONES un total de 10
	media_sim=data.frame(matrix(0,10,nitem))
	#Empezar ciclo de 10 simulaciones
	nsimula=10
	media_sim=matrix(0,10,nitem)
	for (ciclo in 1:nsimula) {
		print (c("simulacion...", ciclo))
		p <- 1/(1+exp(-a*(pred$z1-b)))
		aleat <- data.frame(matrix(runif(nitem*nsuj, 0, 1),ncol=nitem))
		aleat$p=p
		apilado=apila.los.datos.fnc(aleat, fac.intra=list(item=1:nitem), fac.inter='p',
			col.empieza.mr=1, silente=TRUE)
		apilado=ordena.por.variable.fnc(apilado, variable='sujeto', silente=T)
		itemsim=divide.por.factor.fnc(apilado, que.factor='sujeto', silente=T)
		itemsim=data.frame(do.call(rbind,lapply(itemsim, function(x) as.numeric(x$vd < x$p))))
		#itemsim$suma=apply(itemsim,1,function(x) sum(x)/nsimula)
		itemsim$suma=apply(itemsim,1,function(x) sum(x))
		itemsim$niveles=pred$niveles	
		apilado=apila.los.datos.fnc(itemsim, fac.intra=list(item=1:nitem), fac.inter='niveles',
			col.empieza.mr=1, silente=TRUE)
		agregado=agrega.los.datos.fnc(apilado, que.factor=c('item','niveles'),silente=T)
		media_sim=media_sim+matrix(agregado$vd,ncol=nitem)
	}
	media_sim=media_sim/nsimula

	#Analiza Residuos de simulaciones
	for(i in 1:nitem){
		p=lista[[i]]$pt
		residuos=(media_sim[,i]-p)/ sqrt(p*(1-p)/n[,2])
		ertip= sqrt(p*(1-p)/n[,2])
		xires=n[,2]*(media_sim[,i]-p)^ 2/( p*(1-p))
		ji2=sum(xires)
		gl=categorias-npar
		prob= pchisq(c(ji2), df=gl, lower.tail=FALSE)
   		#? que nombre darle ???
  		Item.sim=cbind(i,mediat,n[,2],media_sim[,i],p, ertip,residuos,xires) 
  		names(Item.sim) =c('item','nivel','Z','n' ,'po', 'pt', 'er_tip', 'r_stand', 'chi2')
   		Itemji.sim= data.frame(cbind(i,ji2, gl, prob))
   		names(Itemji.sim)= c('item','chi2','gl','p > chi2')
  		lista.sim[[i]]=Item.sim
  		listaji.sim[[i]]=Itemji.sim
	}
	todos.sim=do.call(rbind,lista.sim)
	todos.sim$ID=row.names(todos.sim)
	#print(todos.sim)
	todos.simji=do.call(rbind,listaji.sim)

	## --------------------------------------------
	## IMPRIME GRAFICOS
	## --------------------------------------------
	if(to.pdf){ # SI SALIDA A ARCHIVO PDF
		if(is.na(nombre[1])){
		   nombre.file='graficas_residuales_tri.pdf'
	  	   name.file=nombre.file
		}else{
		   nombre.file=paste('graficas_residuales_tri_',nombre,'.pdf',sep='')
		   name.file=nombre.file
		}
		if(apaisado){
			width=12; height=10; paper='a4r'
		}else{
			width=7; height=7; paper='a4'
		}
		pdf(file= name.file ,width=width,height=height,onefile=TRUE, paper=paper)
	}
	if(!to.pdf) x11()
	histograma.fnc(pred, 'niveles', to.pdf=to.pdf)
	hist(pred$niveles, col='blue', main='Distribucion por Categorias', xlab='Categoria')

	#Dibuja CCI y Residuales
 	for(j in 1:length(lista.secu)) {
		contador=lista.secu[[j]]
   		if(!to.pdf) x11()
  		par(mfrow=c(3,2))
  		for(i in contador){
			que.plot=lista[[i]]
  			errbar(que.plot$Z, que.plot$pt, que.plot$pt + que.plot$er_tip,
				que.plot$pt-que.plot$er_tip ,
 				main=paste("CCI Item", i), ylim=c(0, 1),
				 xlab="Theta", ylab="P(Z)", pch=19, type='l', col='blue')
  			lines(que.plot$Z, que.plot$po, type='p', pch=21, bg='red')
   		} 
		if(!to.pdf) x11() 
  		par(mfrow=c(3,2))
  		for(i in contador){
			que.plot=lista[[i]]
			plot(que.plot$Z, que.plot$r_stand, main=paste("Distribucion de Residuos Item", i), 
			xlab="Theta", ylab="Residuos Estandarizados", pch=19) 
  			lines(que.plot$Z, rep(0,10), type='l')
			abline(h=c(-2,2),lty=3, col='red')
 		}
		if(j==length(lista.secu)) par(mfrow=c(1,1))
 	}

	#Grafica comparativa P. Observadas en datos reales y simulaciones
	if(!to.pdf) x11()
	f1=data.frame(Puntuaciones=c(sumobs1,itemsim[,nitem+1]),
		grupo=rep(c('observadas','simuladas'),each=nsuj))
	histograma.fnc(f1, vd='Puntuaciones', que.factor='grupo', check=T, to.pdf=to.pdf)
   	par(mfrow=c(1,2))
   		hist(sumobs1, col='blue', main='Puntuaciones Observadas', xlab=' ')
   		hist(itemsim[,nitem+1], col='red', main='Puntuaciones Simuladas', xlab = ' ')
	
	# Grafica de residuos simulados vs reales
	if(!to.pdf) x11() 
    	f2=data.frame(Puntuaciones=c(todos$r_stand,todos.sim$r_stand),
		grupo=rep(c('observadas','simuladas'),each=nitem*10) )
	histograma.fnc(f2, vd='Puntuaciones', que.factor='grupo', check=T, to.pdf=to.pdf)
 	par(mfrow=c(2,1))
		hist(todos$r_stand,breaks=12, col="blue", 
			xlim=c( min(c(todos$r_stand,todos.sim$r_stand),na.rm=TRUE),
			max(c(todos$r_stand,todos.sim$r_stand),na.rm=TRUE)), xlab='Residuos', main= 'Observados')
 		hist(todos.sim$r_stand,breaks=12, col="red",
			xlim=c(min(c(todos$r_stand,todos.sim$r_stand),na.rm=TRUE),
			max(c(todos$r_stand,todos.sim$r_stand),na.rm=TRUE)), xlab='Residuos', main='Simulados')

	# Grafica de chi2 simulados vs reales
	if(!to.pdf) x11() 
	ylim1=c(min(c(todosji$chi2,todos.simji$chi2),na.rm=TRUE),
			max(c(todosji$chi2,todos.simji$chi2),na.rm=TRUE))
	plot(todosji$item,todosji$chi2, type='o', ylim=ylim1, col='blue', xlab='Items', ylab=expression(chi^2))
		text(todosji$item,todosji$chi2, todosji$item, cex=0.6, pos=1, col="blue") 
		lines(todos.simji$item,todos.simji$chi2, type='o', ylim=ylim1, col='red')
		text(todos.simji$item,todos.simji$chi2, todosji$item, cex=0.6, pos=1, col="red") 
		title("Ajuste de Items")
		legend("topleft", c('Observados', 'Simulados'), lty=c(1:2), col=c('blue','red'), bty='n')

	# Grafica de chi2 por niveles
	chir =by(todos$chi2, todos$nivel, function(x) mean(x,na.rm=TRUE))
	chis= by(todos.sim$chi2, todos.sim$nivel, function(x) mean(x,na.rm=TRUE))
	if(!to.pdf) x11() 
 	ylim1=c(min(c(chir,chis),na.rm=TRUE),max(c(chir,chis),na.rm=TRUE))
 	plot(n[,1],chir, type='o', ylim=ylim1, col='blue', xlab='Niveles', ylab=expression(chi^2))
 		text(n[,1],chir, n[,1], cex=0.6, pos=1, col="blue") 
 		lines(n[,1],chis, type='o', ylim=ylim1, col='red')
 		text(n[,1],chis, n[,1], cex=0.6, pos=1, col="red") 
 		title('Estadistico por niveles')
 		legend("topleft", c('Observados', 'Simulados'), lty=c(1:2), col=c('blue','red'), bty='n')

	if(to.pdf) {
	    dev.off()
	    cat('*** Se ha creado el archivo pdf',nombre.file,'en el directorio activo    ***',fill=TRUE)
	}
	## --------------------------------------------
	## FIN IMPRESION GRAFICA
	## --------------------------------------------
 }
# -------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
#  contraste.binomial.fnc(er.api)
#  contraste.binomial.fnc(er, apilados=F)
# -------------------------------------------------------------------------------
 contraste.binomial.fnc=function(datos=NA, vd='vd', variables=NA, p=0.5, apilados=TRUE){
	if(class(try(is.na(datos)))=='logical') {
		cat('',fill=TRUE)
	    crea.cat.fnc('TEST BINOMIAL EXACTO')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat('# Contraste binomial con datos sin apilar                      		',fill=TRUE)
		cat("  contraste.binomial.fnc(datos, apilados=F, variables=1:66)        	",fill=TRUE)
		cat('# Contraste binomial con datos apilados sobre vd como nombre de variable ',fill=TRUE)
		cat("  contraste.binomial.fnc(datos.ap)                                  	",fill=TRUE)
		cat("  contraste.binomial.fnc(datos.ap, vd='aciertos')                     	",fill=TRUE)
		cat('',fill=TRUE)
	    cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Lleva a cabo el contraste por sujeto de que una determinada variable', fill=TRUE)
		cat(' (por defecto se asume vd si los datos estan apilados) sigue la   ', fill=TRUE)
		cat(' distribucion binomial con parametros x (exitos), n (ensayos) y p   ', fill=TRUE)
		cat(' igual a la probabilidad de acierto por ensayo.                       ', fill=TRUE)
		cat(' Con esta funcion podemos comprobar que sujetos serian sospechosos	', fill=TRUE)
		cat(' de haber respondico a la prueba al azar.                      	', fill=TRUE)
		cat('',fill=TRUE)
		cat(' Estos son los argumentos de la funcion contraste.binomial.fnc:    	', fill=TRUE)
		print(argumentos('contraste.binomial.fnc'))
		cat('---------------------------------------------------------------------', fill=TRUE)
	return('help')
	}
	crea.cat.fnc('TEST BINOMIAL EXACTO')
	cat('',fill=TRUE)
	if(apilados){
	cat('*** La funcion asume que los datos de entrada estan apilados.       ***',fill=TRUE)
	cat('*** si no lo estuvieran deberas introducir el argumento apilados=F  ***',fill=TRUE)
	cat('',fill=TRUE)
	}

	if(apilados & !is.na(variables[1])){
		cat('',fill=TRUE)
		cat('*** Error. Si los datos estan apilados debes usar el argumento vd ***',fill=TRUE)
		cat('*** y no el argumento variables.                                  ***',fill=TRUE)
		cat('',fill=TRUE)
	   stop( )
	   }
	if(!apilados & is.na(variables[1])){
		cat('',fill=TRUE)
		cat('*** Error. Si los datos NO estan apilados debes usar el argumento ***',fill=TRUE)
		cat('*** variables y no el argumento vd.                               ***',fill=TRUE)
		cat('',fill=TRUE)
	   stop( )
	}

	# SI APILADOS
	if(apilados){
	   valores=unique(datos[,vd])
	   chivato=match(c(0,1),valores)
	   if(sum(is.na(valores))!=0){
		cat('',fill=TRUE)
		cat('*** Error. La variable a testar debe tener 0 y 1 como valores. ***',fill=TRUE)
		cat('*** Estos son sus principales estadisticos de resumen.         ***',fill=TRUE)
		cat('',fill=TRUE)
		print(summary(datos$vd))
	   stop( )
	   }
	# FIN SI APILADOS

	# SI NO APILADOS
	}else{
	   nombres=names(datos)
	   if(is.na(variables[1])){
	     variables=nombres
	   }else{
	     if(is.numeric(variables)) variables=nombres[variables]
	     datos=data.frame(datos[,variables])
	   }
	   nsuj=dim(datos)[1]
	   x.fila=stack(datos)
	   x.fila$sujeto=factor(paste('suj',1:nsuj,sep=''),levels=paste('suj',1:nsuj,sep=''))
	   names(x.fila)=c('vd','vi','sujeto')
	   datos=x.fila
	   valores=unique(datos[,vd])
	   chivato=match(c(0,1),valores)
	   if(sum(is.na(valores))!=0){
		cat('',fill=TRUE)
		cat('*** Error. La variable a testar debe tener 0 y 1 como valores. ***',fill=TRUE)
		cat('*** Estos son sus principales estadisticos de resumen.         ***',fill=TRUE)
		cat('',fill=TRUE)
		print(summary(datos$vd))
	    stop( )
	   }
	}
	# FIN SI NO APILADOS

 	x.sujeto=divide.por.factor.fnc(datos, que.factor='sujeto',silente=TRUE)
 	x.sujeto=lapply(x.sujeto, function(x) {
 		aciertos=sum(x[,vd],na.rm=TRUE)
 		ensayos=dim(x)[1]
 		co=binom.test(aciertos,ensayos,p)
	 	exitos=as.numeric(co[[1]])
	 	ensayos=as.numeric(co[[2]])
	 	p.exito=as.numeric(round(co[[5]],2))
	 	h0=as.numeric(co[[6]])
	 	p.val=as.numeric(round(co[[3]],6)) 
	 	icon=round(as.numeric(co[[4]]),2)
	 	data.frame(exitos=exitos,ensayos=ensayos,
		p.exito=p.exito,h0=h0,ic.inf=icon[1],ic.sup=icon[2],p.val=p.val)
 	})
	
	x.sujeto=do.call(rbind,x.sujeto)
	print(list(Test.exacto.binomial=x.sujeto))
 }
# -------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
# datos2=recuento.fnc(datos, variables=1:5, criterio=c(0,1))
# datos=data.frame(abs(round(mvrnorm(100,rep(0,5),Sigma=diag(1,5)),0)))
# head(datos2)
 recuento.fnc=function(datos=NA, variables=NA, criterio=NA, new.var=NA, silente=FALSE){

	if(class(try(is.na(datos)))=='logical') {
		cat('',fill=TRUE)
		crea.cat.fnc('recuento.fnc')
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat("  datos= recuento.fnc.fnc(OBrienKaiser, variables=3:17, criterio=0:1)   ",fill=TRUE)
		cat("  datos= recuento.fnc.fnc(OBrienKaiser, variables=3:17, criterio=c(2,4),",fill=TRUE)
		cat("               new.var='mi.recuento'                                    ",fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		cat(' Genera una nueva variable de recuento de la frecuencia absoluta de   ', fill=TRUE)
		cat(' aparicion de un valor o valores explicitados previamente por el usuario', fill=TRUE)
		cat(' mediante el argumento criterio. Si no se utiliza el argumento new.var', fill=TRUE)
		cat(' se generara automaticamente una nueva variable de nombre recuento	', fill=TRUE)
		cat(" ", fill=TRUE)
		cat(" sites.google.com/site/ullrtoolbox/02-manipulacion-de-datos/recuento-fnc", fill=TRUE)
		cat('---------------------------------------------------------------------', fill=TRUE)
		print(argumentos('recuento.fnc'))
	return('help')
	}
	if(!silente) crea.cat.fnc('Recuento de variables')
	if(is.na(criterio[1])){
		cat('',fill=TRUE)
		cat('*** Error. Es obligatorio incluir el argumento criterio ***',fill=TRUE)
		cat("*** Ej. criterio=0, criterio=4:5, criterio='Si', etc ***",fill=TRUE)
		cat('',fill=TRUE)
		stop( )
	}
	nombres=names(datos)
	datos$index=1:dim(datos)[1]
	if(is.na(variables[1])) variables=nombres
	if(is.numeric(variables)) variables=nombres[variables]
	if(is.na(new.var[1])) new.var='recuento'
	n.criterio=length(criterio)
	dat=datos[,c('index',variables)]
	
	if(n.criterio==1){
		for(i in variables){
			if(is.factor(dat[,i]) | is.character(dat[,i])){
				if(is.factor(dat[,i])) dat[,i]=as.character(dat[,i])
				dat[,i]=recode(dat[,i],"NA='-999'")
				dat[dat[,i]!=criterio,i]='-999'
				dat[dat[,i]==criterio,i]=1
				dat[,i]=as.numeric(dat[,i])
			}else{
				dat[,i]=recode(dat[,i],"NA=-999")
				dat[dat[,i]!=criterio,i]=-999
				dat[dat[,i]==criterio,i]=1
			}
		}
	}
	if(n.criterio==2){
		for(i in variables){
			if(is.factor(dat[,i]) | is.character(dat[,i])){
				if(is.factor(dat[,i])) dat[,i]=as.character(dat[,i])
				dat[,i]=recode(dat[,i],"NA='-999'")
				dat[dat[,i]!=criterio[1] & dat[,i]!=criterio[2],i]='-999'
				dat[dat[,i]==criterio[1] | dat[,i]==criterio[2],i]=1
				dat[,i]=as.numeric(dat[,i])
			}else{
				dat[,i]=recode(dat[,i],"NA=-999")
				dat[dat[,i]!=criterio[1] & dat[,i]!=criterio[2],i]=-999
				dat[dat[,i]==criterio[1] | dat[,i]==criterio[2],i]=1
			}
		}
	}

	if(n.criterio==3){
		for(i in variables){
			if(is.factor(dat[,i]) | is.character(dat[,i])){
				if(is.factor(dat[,i])) dat[,i]=as.character(dat[,i])
				dat[,i]=recode(dat[,i],"NA='-999'") 
				dat[dat[,i]!=criterio[1] & dat[,i]!=criterio[2] & dat[,i]!=criterio[3],i]='-999'
				dat[dat[,i]==criterio[1] | dat[,i]==criterio[2] | dat[,i]==criterio[3],i]=1
			}else{
				dat[,i]=recode(dat[,i],"NA=-999")
				dat[dat[,i]!=criterio[1] & dat[,i]!=criterio[2] & dat[,i]!=criterio[3],i]=-999
				dat[dat[,i]==criterio[1] | dat[,i]==criterio[2] | dat[,i]==criterio[3],i]=1
			}
		}
	}
	if(n.criterio==4){
		for(i in variables){
			if(is.factor(dat[,i]) | is.character(dat[,i])){
				if(is.factor(dat[,i])) dat[,i]=as.character(dat[,i])
				dat[,i]=recode(dat[,i],"NA='-999'")
				dat[dat[,i]!=criterio[1] & dat[,i]!=criterio[2] & dat[,i]!=criterio[3]
					& dat[,i]!=criterio[4],i]='-999'
				dat[dat[,i]==criterio[1] | dat[,i]==criterio[2] | dat[,i]==criterio[3]
					| dat[,i]==criterio[4],i]=1
			}else{
				dat[,i]=recode(dat[,i],"NA=-999")
				dat[dat[,i]!=criterio[1] & dat[,i]!=criterio[2] & dat[,i]!=criterio[3]
					& dat[,i]!=criterio[4],i]=-999
				dat[dat[,i]==criterio[1] | dat[,i]==criterio[2] | dat[,i]==criterio[3]
					| dat[,i]==criterio[4],i]=1
			}
		}
	}
	if(n.criterio==5){
		for(i in variables){ 
			if(is.factor(dat[,i]) | is.character(dat[,i])){
				if(is.factor(dat[,i])) dat[,i]=as.character(dat[,i])
				dat[,i]=recode(dat[,i],"NA='-999'")
				dat[dat[,i]!=criterio[1] & dat[,i]!=criterio[2] & dat[,i]!=criterio[3]
					& dat[,i]!=criterio[4] & dat[,i]!=criterio[5],i]='-999'
				dat[dat[,i]==criterio[1] | dat[,i]==criterio[2] | dat[,i]==criterio[3]
					| dat[,i]==criterio[4] | dat[,i]==criterio[5],i]=1
			}else{
				dat[,i]=recode(dat[,i],"NA=-999")
				dat[dat[,i]!=criterio[1] & dat[,i]!=criterio[2] & dat[,i]!=criterio[3]
					& dat[,i]!=criterio[4] & dat[,i]!=criterio[5],i]=-999
				dat[dat[,i]==criterio[1] | dat[,i]==criterio[2] | dat[,i]==criterio[3]
					| dat[,i]==criterio[4] | dat[,i]==criterio[5],i]=1
			}
		}
	}

	if(n.criterio==6){
		for(i in variables){ 
			if(is.factor(dat[,i]) | is.character(dat[,i])){
				if(is.factor(dat[,i])) dat[,i]=as.character(dat[,i])
				dat[,i]=recode(dat[,i],"NA='-999'")
				dat[dat[,i]!=criterio[1] & dat[,i]!=criterio[2] & dat[,i]!=criterio[3]
					& dat[,i]!=criterio[4] & dat[,i]!=criterio[5]
					& dat[,i]!=criterio[6],i]='-999'
				dat[dat[,i]==criterio[1] | dat[,i]==criterio[2] | dat[,i]==criterio[3]
					| dat[,i]==criterio[4] | dat[,i]==criterio[5]
					| dat[,i]==criterio[6],i]=1
			}else{
				dat[,i]=recode(dat[,i],"NA=-999")
				dat[dat[,i]!=criterio[1] & dat[,i]!=criterio[2] & dat[,i]!=criterio[3]
					& dat[,i]!=criterio[4] & dat[,i]!=criterio[5]
					& dat[,i]!=criterio[6],i]=-999
				dat[dat[,i]==criterio[1] | dat[,i]==criterio[2] | dat[,i]==criterio[3]
					| dat[,i]==criterio[4] | dat[,i]==criterio[5]
					| dat[,i]==criterio[6],i]=1
			}
		}
	}
	if(n.criterio==7){
		for(i in variables){ 
			if(is.factor(dat[,i]) | is.character(dat[,i])){
				if(is.factor(dat[,i])) dat[,i]=as.character(dat[,i])
				dat[,i]=recode(dat[,i],"NA='-999'")
				dat[dat[,i]!=criterio[1] & dat[,i]!=criterio[2] & dat[,i]!=criterio[3]
					& dat[,i]!=criterio[4] & dat[,i]!=criterio[5]
					& dat[,i]!=criterio[6] & dat[,i]!=criterio[7],i]='-999'
				dat[dat[,i]==criterio[1] | dat[,i]==criterio[2] | dat[,i]==criterio[3]
					| dat[,i]==criterio[4] | dat[,i]==criterio[5]
					| dat[,i]==criterio[6] | dat[,i]==criterio[7],i]=1
			}else{
				dat[,i]=recode(dat[,i],"NA=-999")
				dat[dat[,i]!=criterio[1] & dat[,i]!=criterio[2] & dat[,i]!=criterio[3]
					& dat[,i]!=criterio[4] & dat[,i]!=criterio[5]
					& dat[,i]!=criterio[6] & dat[,i]!=criterio[7],i]=-999
				dat[dat[,i]==criterio[1] | dat[,i]==criterio[2] | dat[,i]==criterio[3]
					| dat[,i]==criterio[4] | dat[,i]==criterio[5]
					| dat[,i]==criterio[6] | dat[,i]==criterio[7],i]=1
			}
		}
	}
	if(n.criterio==8){
		for(i in variables){ 
			if(is.factor(dat[,i]) | is.character(dat[,i])){
				if(is.factor(dat[,i])) dat[,i]=as.character(dat[,i])
				dat[,i]=recode(dat[,i],"NA='-999'")
				dat[dat[,i]!=criterio[1] & dat[,i]!=criterio[2] & dat[,i]!=criterio[3]
					& dat[,i]!=criterio[4] & dat[,i]!=criterio[5]
					& dat[,i]!=criterio[6] & dat[,i]!=criterio[7] & dat[,i]!=criterio[8],i]='-999'
				dat[dat[,i]==criterio[1] | dat[,i]==criterio[2] | dat[,i]==criterio[3]
					| dat[,i]==criterio[4] | dat[,i]==criterio[5]
					| dat[,i]==criterio[6] | dat[,i]==criterio[7] | dat[,i]==criterio[8],i]=1
			}else{
				dat[,i]=recode(dat[,i],"NA=-999")
				dat[dat[,i]!=criterio[1] & dat[,i]!=criterio[2] & dat[,i]!=criterio[3]
					& dat[,i]!=criterio[4] & dat[,i]!=criterio[5]
					& dat[,i]!=criterio[6] & dat[,i]!=criterio[7] & dat[,i]!=criterio[8],i]=-999
				dat[dat[,i]==criterio[1] | dat[,i]==criterio[2] | dat[,i]==criterio[3]
					| dat[,i]==criterio[4] | dat[,i]==criterio[5]
					| dat[,i]==criterio[6] | dat[,i]==criterio[7] | dat[,i]==criterio[8],i]=1
			}
		}
	}
	if(n.criterio==9){
		for(i in variables){ 
			if(is.factor(dat[,i]) | is.character(dat[,i])){
				if(is.factor(dat[,i])) dat[,i]=as.character(dat[,i])
				dat[,i]=recode(dat[,i],"NA='-999'")
				dat[dat[,i]!=criterio[1] & dat[,i]!=criterio[2] & dat[,i]!=criterio[3]
					& dat[,i]!=criterio[4] & dat[,i]!=criterio[5]
					& dat[,i]!=criterio[6] & dat[,i]!=criterio[7] & dat[,i]!=criterio[8]
					& dat[,i]!=criterio[9],i]='-999'
				dat[dat[,i]==criterio[1] | dat[,i]==criterio[2] | dat[,i]==criterio[3]
					| dat[,i]==criterio[4] | dat[,i]==criterio[5]
					| dat[,i]==criterio[6] | dat[,i]==criterio[7] | dat[,i]==criterio[8]
					| dat[,i]==criterio[9],i]=1
			}else{
				dat[,i]=recode(dat[,i],"NA=-999")
				dat[dat[,i]!=criterio[1] & dat[,i]!=criterio[2] & dat[,i]!=criterio[3]
					& dat[,i]!=criterio[4] & dat[,i]!=criterio[5]
					& dat[,i]!=criterio[6] & dat[,i]!=criterio[7] & dat[,i]!=criterio[8]
					& dat[,i]!=criterio[9],i]=-999
				dat[dat[,i]==criterio[1] | dat[,i]==criterio[2] | dat[,i]==criterio[3]
					| dat[,i]==criterio[4] | dat[,i]==criterio[5]
					| dat[,i]==criterio[6] | dat[,i]==criterio[7] | dat[,i]==criterio[8]
					| dat[,i]==criterio[9],i]=1
			}
		}
	}
	if(n.criterio==10){
		for(i in variables){ 
			if(is.factor(dat[,i]) | is.character(dat[,i])){
				if(is.factor(dat[,i])) dat[,i]=as.character(dat[,i])
				dat[,i]=recode(dat[,i],"NA='-999'")
				dat[dat[,i]!=criterio[1] & dat[,i]!=criterio[2] & dat[,i]!=criterio[3]
					& dat[,i]!=criterio[4] & dat[,i]!=criterio[5]
					& dat[,i]!=criterio[6] & dat[,i]!=criterio[7] & dat[,i]!=criterio[8]
					& dat[,i]!=criterio[9] & dat[,i]!=criterio[10],i]='-999'
				dat[dat[,i]==criterio[1] | dat[,i]==criterio[2] | dat[,i]==criterio[3]
					| dat[,i]==criterio[4] | dat[,i]==criterio[5]
					| dat[,i]==criterio[6] | dat[,i]==criterio[7] | dat[,i]==criterio[8]
					| dat[,i]==criterio[9] | dat[,i]==criterio[10],i]=1
			}else{
				dat[,i]=recode(dat[,i],"NA=-999")
				dat[dat[,i]!=criterio[1] & dat[,i]!=criterio[2] & dat[,i]!=criterio[3]
					& dat[,i]!=criterio[4] & dat[,i]!=criterio[5]
					& dat[,i]!=criterio[6] & dat[,i]!=criterio[7] & dat[,i]!=criterio[8]
					& dat[,i]!=criterio[9] & dat[,i]!=criterio[10],i]=-999
				dat[dat[,i]==criterio[1] | dat[,i]==criterio[2] | dat[,i]==criterio[3]
					| dat[,i]==criterio[4] | dat[,i]==criterio[5]
					| dat[,i]==criterio[6] | dat[,i]==criterio[7] | dat[,i]==criterio[8]
					| dat[,i]==criterio[9] | dat[,i]==criterio[10],i]=1
			}
		}
	}
	if(n.criterio > 10){
		cat('',fill=T)
		cat('*** Error. El numero maximo de criterios para el recuento es 10 ***',fill=T)
# 		cat('',fill=T)
		stop( )
	}
	for(i in variables)	dat[,i]=recode(dat[,i], " 1=1; else=NA")
	dat[,new.var]=apply(dat[,-1],1,function(x) sum(x,na.rm=T))
	datos=merge(datos, dat[,c('index',new.var)], by='index')
	datos=datos[,-1]
	if(!silente){
		cat('*** Se ha creado con exito la variable de recuento:',new.var,fill=TRUE)
		cat('', fill=TRUE)
	}
	return(datos)
 }
# -------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
# Transforma delta en r o r en delta.
# -------------------------------------------------------------------------------
 delta.r.fnc=function(delta=NA, r=NA, n=NA){
	if(is.na(delta) & is.na(r)){
		cat('',fill=TRUE)
		cat('*** Error. Debes introducir un valor para delta o r ***',fill=TRUE)
		stop()
	}

	if(is.na(delta) & !is.na(r)){
		if(is.na(n[1]) | length(n)==1) p=0.5 else p=n[1]/(n[1]+n[2])
		delta=sqrt(r^2/( (p*(1-p))*(1-r^2)))
		cat('',fill=TRUE)
		cat('*** Para r=',r,'(r2=',r^2,') la delta correspondiente es:',fill=TRUE)
		cat('*** delta=',delta,fill=TRUE)
	}

	if(!is.na(delta) & is.na(r)){
		if(is.na(n[1]) | length(n)==1) p=0.5 else p=n[1]/(n[1]+n[2])
		r=delta/sqrt(delta^2+(1/(p*(1-p))))
		cat('',fill=TRUE)
		cat('*** Para delta=',delta,'el valor de r correspondiente es:',fill=TRUE)
		cat('*** r=',r,'(r2=',r^2,')',fill=TRUE)
	}
 }
# -------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
# datos=recode.multiple.fnc(datos, variables=2:8, valor1=c(1,8),valor2=c(0,NA))
# code by Pedro Prieto & Juan A. Hernandez
# -------------------------------------------------------------------------------
 recode.multiple.fnc=function(datos, variables=NA , valor1, valor2, silente=FALSE,
											nueva.variable=FALSE,sustituye=FALSE){
	require(car, quietly=TRUE)
	if(!silente) crea.cat.fnc('RECODIFICACION MULTIPLE DE VARIABLES')
	fin=FALSE; two.steps=FALSE
	
	# MONTAJE DE VARIABLES
	nombres=names(datos)
    if(is.na(variables[1])){
          cat('',fill=TRUE)
          cat('*** Error. Debes indicar al menos una variable a recodificar ***',fill=TRUE) 
          cat("*** Ej. variables=2, variables=c(2,5,10),variables='gender' ***",fill=TRUE)
          cat('',fill=TRUE)
          stop( )
    }
	if(is.numeric(variables[1])) variables=nombres[variables]
	# FIN DE MONTAJE DE VARIABLES
	
	# CHECK DE VALORES ANTIGUOS Y NUEVOS
    n.v1=length(valor1); n.v2=length(valor2)
    if(n.v1 != n.v2){
        if(n.v2 ==1){
            valor2=rep(valor2,n.v1)
        }else{
          cat('',fill=TRUE)
          cat('*** Error. La longitud de los los vectores de valores 1 y 2 deben ser iguales',fill=TRUE)
          cat('*** o el segundo vector de valores contener un unico valor.',fill=TRUE)
          stop( )
          cat('',fill=TRUE)
        }
    }
	# FIN CHECK DE VALORES
    
	if(is.character(valor1) & is.character(valor2)) two.steps=TRUE
	if(nueva.variable & !sustituye){
		new.var=paste(variables,'.r',sep='')
		indice=new.var %in% nombres	
		if(sum(indice)!=0){
			que.var=new.var[indice]
			cat('*** Error. Parece que esas variables han sido anteriormente recodificadas,',fill=TRUE)
			cat('*** puesto que ya existen en la base de datos. Estas son esas variables:',fill=TRUE)
			print(que.var)
			cat('',fill=TRUE)			
			cat('*** Renombralas si deseas conservarlas con cambia.nombre.var.fnc',fill=TRUE)
			cat('*** o incluye el argumento sustituye=T en la llamada a la funcion.',fill=TRUE)
			cat('',fill=TRUE)			
			stop( )
		}		
	}
	if(nueva.variable & sustituye) new.var=paste(variables,'.r',sep='')
	
 	# RECODIFICACION POR VARIABLE Y VALORES SI HAY ALFUNAMERICO EN VALOR1 O VALOR2
    if(is.character(valor1) | is.character(valor2)){
		if(is.numeric(variables[1])) variables=nombres[variables]
		dat_=datos[,variables]
		if(is.character(valor2)) to.factor=TRUE else to.factor=FALSE
		if(!two.steps){
			for(i in 1:length(variables)){
				for(j in 1:length(valor1)){
					antiguo=valor1[j]; nuevo=valor2[j]
					try(assign('antiguo',antiguo,envir=.GlobalEnv),silent=TRUE) 
					try(assign('nuevo',nuevo,envir=.GlobalEnv),silent=TRUE)
					if(to.factor){
						dat_[,i]=recode(dat_[,i], "antiguo=nuevo")
					}else{	
						dat_[,i]=recode(dat_[,i], "antiguo=nuevo",  as.factor.result=FALSE)
					}				
				}
				if(to.factor) dat_=reordena.factor.fnc(dat_, que.factor=variables[i],
					niveles=valor2, hacer.NA=TRUE, silente=TRUE)
			}
		}else{	
			nuevo_=1:length(valor2)
			for(i in 1:length(variables)){
				for(j in 1:length(valor1)){
					antiguo=valor1[j]; nuevo=nuevo_[j]
					try(assign('antiguo',antiguo,envir=.GlobalEnv),silent=TRUE) 
					try(assign('nuevo',nuevo,envir=.GlobalEnv),silent=TRUE)
						dat_[,i]=recode(dat_[,i], "antiguo=nuevo",  as.factor.result=FALSE)
				}
			}
			for(i in 1:length(variables)){
				for(j in 1:length(valor1)){
					antiguo=nuevo_[j]; nuevo=valor2[j]
					try(assign('antiguo',antiguo,envir=.GlobalEnv),silent=TRUE) 
					try(assign('nuevo',nuevo,envir=.GlobalEnv),silent=TRUE)
						dat_[,i]=recode(dat_[,i], "antiguo=nuevo")
				}
				dat_=reordena.factor.fnc(dat_, que.factor=variables[i],
					niveles=valor2, hacer.NA=TRUE, silente=TRUE)				
			}			
		}			
		fin=TRUE
	}
 	# FIN RECODIFICACION PARA VALOR1 O VALOR2 CARACTER

	# RECODIFICACION POR VARIABLE Y VALORES SI NUMERICA EN VALOR1 Y VALOR2
	if(!fin){
		if(is.numeric(valor1) | is.numeric(valor2)){
			dat_=datos[,variables]
			for(j in 1:n.v1){
				exp= paste(valor1[j],'=',valor2[j]+100,sep="")
				for (i in 1:length(variables)){
					variables[i]
					dat_[,variables[i]]=recode(dat_[,variables[i]], exp) 
				}
			}  
			for(j in 1:n.v1){
				exp= paste(valor2[j]+100,'=',valor2[j],sep="")
				for (i in 1:length(variables)){
					variables[i]
					dat_[,variables[i]]=recode(dat_[,variables[i]], exp) 
				}
			}
		}
	}	
	# FIN RECODIFICACION POR VARIABLE Y VALORES SI NUMERICA EN VALOR1 Y VALOR2

	if(nueva.variable){
		names(dat_)=new.var
		datos=cbind(datos,dat_) 
	}else{
		datos[,variables]=dat_
	}				
	
    if(!silente){
		if(nueva.variable){
			cat('*** Se han recodificado las siguientes variables:',fill=TRUE)
			print(variables)
			cat('',fill=TRUE)      			
			cat('*** En estas nuevas variables:',fill=TRUE)
			print(new.var)
			cat('',fill=TRUE)      
			cat('*** desde los valores originales:',valor1,fill=TRUE)
			cat('',fill=TRUE)
			cat('*** a los nuevos:',valor2,fill=TRUE)
			cat('',fill=TRUE)		
		}else{
			cat('*** Se han recodificado las siguientes variables:',fill=TRUE)
			print(variables)
			cat('',fill=TRUE)      
			cat('*** desde los valores originales:',valor1,fill=TRUE)
			cat('',fill=TRUE)
			cat('*** a los nuevos:',valor2,fill=TRUE)
			cat('',fill=TRUE)
		}
    } 
  #try(detach(package:car), silent=TRUE)
  return(datos)
 }	
# -------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
# datos=invierte.valores.fnc=function(datos,lista,vmin,vmax)
# code by Pedro Prieto
# -------------------------------------------------------------------------------
 invierte.valores.fnc=function(datos,lista,vmin,vmax){
  if(vmin > vmax){
  cat('',fill=TRUE)
          cat('*** Error. vmax no puede ser inferior a vmin',fill=TRUE)
          cat('',fill=TRUE)
          stop( )
  return()
  }
  for(i in 1:length(lista)){
  datos[,lista[i]]=(vmax+vmin)-datos[,lista[i]]
  }
  return(datos)
}
# -------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
# Genera codigo latex cuando se incluye el argumento latex=T en la llamada a 
# multiples funciones
# -------------------------------------------------------------------------------
 latex.fnc=function(objeto, titulo=NULL, digitos=NULL){
    require(xtable,  quietly=TRUE)
    if(is.list(objeto) & !is.data.frame(objeto)){
      nombres=names(objeto)
      for(i in 1:length(objeto)){
      	if(i==1) crea.cat.fnc('*** Copia y pega el codigo entre estas lineas en una ventana TextMath de Libreoffice')
		x=objeto[[i]]
		if(is.list(x) & !is.data.frame(x)){
	  		nombres2=paste(nombres[i],names(x),sep=' ')
	  		for(j in 1:length(x)){
	    		new.x=x[[j]]
	    		salida=try(xtable(new.x, caption=titulo, label=titulo, digits=digitos),silent=TRUE)
	    		if(class(salida)[1]!='try-error'){
	      			cat('',fill=TRUE)
	      			cat('*** Codigo latex para:',nombres2[j],fill=TRUE)
	      			cat('------------------------------------------------------------------------------------',fill=TRUE)  	
	      			print(salida)
	      			cat('',fill=TRUE)	    
	    		}else{
	      			cat('',fill=TRUE)	
	      			cat('------------------------------------------------------------------------------------',fill=TRUE)
	      			cat('*** Error. No ha sido posible crear el codigo latex para:',nombres[i],fill=TRUE)
	      			cat('------------------------------------------------------------------------------------',fill=TRUE)
	    		}	    
	  		}
		}
		salida=try(xtable(x, caption=titulo, label=titulo, digits=digitos),silent=TRUE)
		# ABRE IF ERROR
		if(class(salida)[1]!='try-error'){
	  		cat('',fill=TRUE)
	  		cat('*** Codigo latex para:',nombres[i],fill=TRUE)
	  		cat('------------------------------------------------------------------------------------',fill=TRUE)  	
	  		print(salida)
	  		cat('',fill=TRUE)
	  		if(i==length(objeto))
	  			cat('------------------------------------------------------------------------------------',fill=TRUE)  	
		}else{
	  		cat('',fill=TRUE)	
	  		cat('*** Error. No ha sido posible crear el codigo latex para:',nombres[i],fill=TRUE)
	  		cat('------------------------------------------------------------------------------------',fill=TRUE)
	  		cat('------------------------------------------------------------------------------------',fill=TRUE)
		}# CIERRA IF ERROR
     } # CIERRA FOR DESDE 1 A LENGTH OBJETO
	# SI EL OBJETO NO ES UNA LISTA 
    }else{
    	crea.cat.fnc('*** Copia y pega el codigo entre estas lineas en una ventana TextMath de Libreoffice')
      	salida=try(xtable(objeto, caption=titulo, label=titulo, digits=digitos),silent=TRUE)
      	print(salida)
      	cat('',fill=TRUE)
		cat('------------------------------------------------------------------------------------',fill=TRUE)
    }
  try(detach(package:xtable),silent=TRUE)
 }

# -------------------------------------------------------------------------------
# extremo.multivariado.fnc(bfi, variables=1:25)
# -------------------------------------------------------------------------------
  extremo.multivariado.fnc=function(datos, variables=NA, n=10, silente=FALSE){
    nombres=names(datos)
    if(!is.na(variables[1])){
      if(is.numeric(variables[1])) variables=nombres[variables]
      datos=datos[,variables]
    }else{
      variables=names(datos)
    }
    originales=datos[,variables]
    datos$registro=1:dim(datos)[1]
    datos=na.omit(datos)
    if(dim(datos)[1]==0){
       cat('',fill=TRUE)
       crea.cat.fnc('Deteccion de casos extremos multivariados')
       cat('*** Error. Al eliminar los casos perdidos la matriz listwise pierde ***',fill=TRUE)
       cat('*** todos los sujetos. ***',fill=TRUE)
       cat('',fill=TRUE)
       stop()
    }
    Sx = correlacion.fnc(datos, variables=variables, covarianza=TRUE, silente=TRUE)
    D2 = mahalanobis(datos[,variables], colMeans(datos[,variables],na.rm=TRUE), cov=Sx)
    D2=data.frame(D2=D2, registro=datos$registro)
    D2$D=sqrt(D2$D2)
    D2$p.val=round(1-pchisq(D2$D2,dim(Sx)[1]),4)
    D2=ordena.por.variable.fnc(D2, variable=3, descendente=TRUE, silente=TRUE)
    D2$p.val=format(D2$p.val, digits=4, scientific=F)    
    D2=D2[,c(3,1,4,2)]    
    D2_=subset(D2, p.val <= 0.001)
    n.extremos=dim(D2_)[1]
    if(dim(D2_)[1]==0) D2_=head(D2)
    if(!silente){
      crea.cat.fnc('Deteccion de casos extremos multivariados')
      if(dim(datos)[1] < 10*dim(datos)[2]){
	cat('',fill=TRUE)
	cat('*** Warning. Dispones de menos de 10 casos por variables. La solucion',fill=TRUE)
	cat('*** multivariada a estimar puede ser inestable y de escasa replicabilidad.',fill=TRUE)
	cat('',fill=TRUE)
      }      
      cat('',fill=TRUE)
	n.ext=paste('Se han detectado ',n.extremos,' (D2 p <0.001) registros potencialmente problematicos.',sep='')
      	n.reg=paste('Distancias calculadas desde una matriz de ',dim(datos)[1],' registros en ',dim(datos)[2]-1,' variables',sep='')
      	n.ori=paste('obtenida de la matriz original de ',dim(originales)[1],' registros en ',dim(originales)[2],' variables.',sep='')
      cat('***',n.reg,fill=TRUE)
      cat('***',n.ori,fill=TRUE)
      cat('',fill=TRUE)
	cat('***',n.ext, fill=TRUE)
      cat('',fill=TRUE)      
      cat('*** Registros con distancia de Mahalanobis. Prueba a eliminar aquellos con',fill=TRUE)
      cat('*** p < 0.001 y repite el analisis multivariado.', fill=TRUE)
      cat('',fill=TRUE)
      cat('*** Ej:   extremos=extremos.multivariados.fnc(bfi, variables=1:25) ',fill=TRUE)
      cat('***       Manova.fnc(datos[-c(extremos$registro),], variables=6:10,',fill=TRUE)
      cat("         	      fac.inter=c('curso','genero')) ", fill=TRUE)
      cat('',fill=TRUE)
    }    
 return(D2_)
 }
# -------------------------------------------------------------------------------
#
# -------------------------------------------------------------------------------
analisis.mediacion.fnc=function(datos,mediacion, interaccion.vi.m=FALSE, med.mod=NULL, 
                      logit=TRUE, random=NULL, fac.random=NULL, sensibilidad=FALSE){
  require(mediation, quietly=TRUE)
  require(car, quietly=TRUE)
  crea.cat.fnc('ANALISIS DE MEDIACION')
  print(mediacion)
  print(c(interaccion.vi.m=interaccion.vi.m))
  print(c(Mediacion.modulada.por=med.mod))
  cat('',fill=TRUE)
  basica=FALSE; covari=FALSE; interac=FALSE; 
  covari.interac=FALSE; med.mod_=FALSE; med.mod.cova=FALSE; estima.jerarquica=FALSE
  
  # CHECK MODELO
  if(logit) link_='logit' else link_='probit'
  
  if(interaccion.vi.m & !is.null(med.mod)){
    cat('', fill=TRUE)
    cat('*** Error. No puedes estimar al mismo tiempo la interaccion vi:mediacion', fill=TRUE)
    cat('*** y la mediacion moderada vi:covariante. Debes seleccionar una u otra.', fill=TRUE)
    cat('', fill=TRUE)
    stop( )
  }
  
  if(!is.null(med.mod) & !is.list(med.mod)){
    cat('', fill=TRUE)
    cat('*** Error. El argumento med.mod debe ser obligatoriamente una lista,', fill=TRUE)
    cat('*** en cuyo interior aparecera como minimo el nombre de la variable', fill=TRUE)
    cat('*** moduladora. El usuario debe indicar ademas aquellos dos valores', fill=TRUE)
    cat('*** sobre los que se desea comprobar dicha modulacion. Si se omitiesen', fill=TRUE)
    cat('*** incluyendo como valor NA, la funcion lo hara sobre los cuartiles', fill=TRUE)
    cat('*** primero y tercero (Q1 y Q3).', fill=TRUE)
    cat('', fill=TRUE)
    cat("*** Ej. med.mod=list(age=c(20,60)) ", fill=TRUE)
    cat("*** Ej. med.mod=list(age=NA) # Se hara sobre Q1 y Q3 de edad ", fill=TRUE)	      
    cat("*** Ej. med.mod=list(estado.civil=c(1,3))", fill=TRUE)	      
    cat("*** Ej. med.mod=list(estado.civil=c('casado','divorciado'))", fill=TRUE)	      
    cat('', fill=TRUE)
    stop()
  }
  if(!is.null(med.mod) & is.list(med.mod)){
    cova=names(med.mod)
    if(is.na(med.mod[[1]][1])){
      if(is.factor(datos[,cova]) | is.character(datos[,cova])){
        niveles=names(frecuencias.fnc(datos,cova,silente=TRUE)[[1]]$tabla)	  
        mensaje=paste('*** Se utilizaran los niveles ',niveles[1],' y ',niveles[2],' del factor ',cova,' ***',sep='')
        cat('', fill=TRUE)
        cat(mensaje,fill=TRUE)
        cat('*** en la mediacion modulada.',fill=TRUE)
        cat('', fill=TRUE)		    
      }else{
        cat('', fill=TRUE)
        cat('*** Dado que no has indicado dos puntos de corte para la covariante en la', fill=TRUE)
        cat('*** mediacion modulada. Se realizara comparando el primer cuartil vs', fill=TRUE)
        cat('*** el tercero para la variable definida (Q1 vs Q3).', fill=TRUE)
        cat('', fill=TRUE)
      }
    }
  }
  
  if(is.null(mediacion$covariante) & !interaccion.vi.m & is.null(med.mod)) basica=TRUE
  if(!is.null(mediacion$covariante) & !interaccion.vi.m & is.null(med.mod)) covari=TRUE
  if(interaccion.vi.m) interac=TRUE
  if(interaccion.vi.m & !is.null(mediacion$covariante)) {interac=FALSE; covari.interac=TRUE}
  if(!is.null(med.mod)) med.mod_=TRUE
  if(med.mod_ & !is.null(mediacion$covariante)) med.mod.cova=TRUE
  
  if(basica){
    variables=c(mediacion$vd,mediacion$vi,mediacion$m)
    check.variables.fnc(datos,variables)
    mod.m=paste(mediacion$m,' ~ ',mediacion$vi,sep='')
    mod.vd=paste(mediacion$vd,' ~ ',mediacion$m,'+',mediacion$vi,sep='')
  }
  if(covari){
    cova=mediacion$covariante
    variables=c(mediacion$vd,mediacion$vi,mediacion$m,cova)
    check.variables.fnc(datos,variables)	  
    mod.m=paste(mediacion$m,' ~ ',cova[1],sep='')
    mod.vd=paste(mediacion$vd,' ~ ',cova[1],sep='')
    if(length(cova) > 1){
      for(i in 2:length(cova)) {
        mod.m=paste(mod.m,'+',cova[i],sep='')
        mod.vd=paste(mod.vd,'+',cova[i],sep='')
      }
    }
    mod.m=paste(mod.m,' + ',mediacion$vi,sep='')
    mod.vd=paste(mod.vd,' + ',mediacion$vi,' + ',mediacion$m,sep='')	  
    
  }
  if(interac){
    variables=c(mediacion$vd,mediacion$vi,mediacion$m)
    check.variables.fnc(datos,variables)	
    mod.m=paste(mediacion$m,' ~ ',mediacion$vi,sep='')
    mod.vd=paste(mediacion$vd,' ~ ',mediacion$vi,'*',mediacion$m,sep='')	     
  } 
  if(covari.interac){
    cova=mediacion$covariante
    variables=c(mediacion$vd,mediacion$vi,mediacion$m,cova)
    check.variables.fnc(datos,variables)	  
    n.cova=length(mediacion$covariante)
    cova=mediacion$covariante[1]
    if(n.cova > 1) { for(i in 2:n.cova) cova=paste(cova,'+',mediacion$cova[i],collapse='+')}	
    mod.m=paste(mediacion$m,' ~ ',cova,' + ',mediacion$vi,sep='')
    mod.vd=paste(mediacion$vd,' ~ ',cova,' + ',mediacion$vi,'*',mediacion$m,sep='')
  }
  if(med.mod_){
    cova=names(med.mod)
    variables=c(mediacion$vd,mediacion$vi,mediacion$m,cova)
    check.variables.fnc(datos,variables)	  	
    covari=names(med.mod); n.covari=length(covari)
    if(n.covari > 1) {
      cat('',fill=TRUE)
      cat('*** Error. La covariante moduladora solo puede ser una.',fill=TRUE)
      cat('*** Edita y modifica apropiadamente el argumento med.mod',fill=TRUE)
      cat('',fill=TRUE)
      stop( )
    }   
    mod.m=paste(mediacion$m,' ~ ',covari,'*',mediacion$vi, sep='')
    mod.vd=paste(mediacion$vd,' ~ ',covari,'*',mediacion$vi,' + ',covari,'*',mediacion$m,sep='')
  }
  if(med.mod.cova){
    cova1=names(med.mod)
    cova2=mediacion$covariante
    variables=c(mediacion$vd,mediacion$vi,mediacion$m,cova1,cova2)
    check.variables.fnc(datos,variables)	  		
    
    covari=names(med.mod); n.covari=length(covari)
    if(n.covari > 1) {
      cat('',fill=TRUE)
      cat('*** Error. La covariante moduladora solo puede ser una.',fill=TRUE)
      cat('*** Edita y modifica apropiadamente el argumento med.mod',fill=TRUE)
      cat('',fill=TRUE)
      stop( )
    }   	    
    n.cova=length(mediacion$covariante)
    cova=mediacion$covariante[1]
    if(n.cova > 1) { for(i in 2:n.cova) cova=paste(cova,'+',mediacion$cova[i],collapse='+')}		
    mod.m=paste(mediacion$m,' ~ ',cova,' + ',covari,'*',mediacion$vi, sep='')
    mod.vd=paste(mediacion$vd,' ~ ',cova,'+',covari,'*',mediacion$vi,' + ',covari,'*',mediacion$m,sep='')
  }

  # CHECK SI JERARQUICA LMER
  if(!is.null(random[1])){
    exist.id=match(random,names(datos));
    if(is.na(exist.id[1])){
      cat('',fill=TRUE)
      etiq=paste('*** Error. No existe la variable de jerarquia incluida: ',random,' ***',sep='')
      cat(etiq,fill=TRUE)
      cat('',fill=TRUE)
      stop( )
    }
    estima.jerarquica=TRUE
    variables=c(variables,random)
    check.random=data.frame(frecuencias.fnc(datos, random, silente=T)[[1]]$tabla)
    n.lev=dim(check.random)[1]
    freq1=subset(check.random, Freq == 1)
    if(dim(freq1)[1]==n.lev){
      cat('',fill=TRUE)
      etiq=paste('*** Error. La variable que has incluido en el argumento random: ',
      	random,' no parece aleatoria ***',sep='')
      cat(etiq,fill=TRUE)
      cat('',fill=TRUE)
	  stop()
	}
	try(detach(package=lmerTest), silent=TRUE)
    require(lme4, quietly=TRUE)
  }
  if(!is.null(random[1]) & !is.null(fac.random[1])){
    exist.fac.random=match(fac.random,names(datos));
    if(is.na(exist.fac.random[1])){
      cat('',fill=TRUE)
      etiq=paste('*** Error. No existe el factor random: ',fac.random,' en la base de datos ***',sep='')
      cat(etiq,fill=TRUE)
      cat('',fill=TRUE)
      stop( )
    }    
  }
  # FIN SI LMER

  # CHECK MISSING VALUES
  antes=dim(datos)[1]; datos=na.omit(datos[,variables])
  desp=dim(datos)[1]
  n.mis=antes-desp
  a1=paste('*** Se estima el modelo de mediacion sobre un total de ',desp,' casos validos.',sep='')
  a2=paste('*** Se han eliminado del analisis por casos perdidos ',n.mis,' observaciones.',sep='')
  cat('',fill=TRUE)
  cat(a1,fill=TRUE)
  cat(a2,fill=TRUE)
  cat('',fill=TRUE)
  
  freq.vd=frecuencias.fnc(datos, variable=mediacion$vd, silente=TRUE)[[1]]$tabla
  freq.m=frecuencias.fnc(datos, variable=mediacion$m, silente=TRUE)[[1]]$tabla
  lineal.vd=TRUE; lineal.m=TRUE; met.est.vd='Modelo lineal'; met.est.m=met.est.vd
  
  # DEFINE ETIQUETAS DE MODELOS
  # VD Y M
  if(is.null(random)){
    if(length(freq.vd)==2){ 
      lineal.vd=FALSE;
      met.est.vd=paste('glm binomial ',link_,sep='')
    }
    if(length(freq.m)==2){ 
      lineal.m=FALSE
      met.est.m=paste('glm binomial ',link_,sep='')
    }
    metodo.est=list(estimacion.mediadora.dependiente=met.est.m,
                  estimacion.vd.dependiente=met.est.vd)
  }else{
    if(length(freq.vd)==2){     
      lineal.vd=FALSE;
      if(is.character(datos[,mediacion$vd]) | is.factor(datos[,mediacion$vd])){
		datos[,mediacion$vd]=as.numeric(as.factor(datos[,mediacion$vd]))
		datos[,mediacion$vd]=datos[,mediacion$vd]-1
		cat('',fill=TRUE)
		pega=paste('*** WARNIGN. Se ha recodificado internamente la variable dependiente: ',mediacion$vd,sep='')
		cat(pega,fill=TRUE)
		cat('*** a los nuevos valores numericos 0 y 1 (el modelo glmer asi lo exige).' ,fill=TRUE)	
		cat('',fill=TRUE)
      }      
      met.est.vd=paste('glmer binomial ',link_,sep='')
    }
    
    if(length(freq.vd) > 2 & is.numeric(datos[,mediacion$vd])) met.est.vd='Lineal jerarquico'     
    
    if(length(freq.m)==2){ 
      lineal.m=FALSE
      if(is.character(datos[,mediacion$m]) | is.factor(datos[,mediacion$m])){
		datos[,mediacion$m]=as.numeric(as.factor(datos[,mediacion$m]))
		datos[,mediacion$m]=datos[,mediacion$m]-1
		cat('',fill=TRUE)
		pega=paste('*** WARNIGN. Se ha recodificado internamente la variable dependiente: ',mediacion$vd,sep='')
		cat(pega,fill=TRUE)
		cat('*** a los nuevos valores numericos 0 y 1 (el modelo glmer asi lo exige).' ,fill=TRUE)	
		cat(,fill=TRUE)
      }
      met.est.m=paste('glmer binomial ',link_,sep='')
    }else{
      lineal.m=TRUE
      met.est.m='Lineal jerarquico'
    }  
    metodo.est=list(estimacion.mediadora.dependiente=met.est.m,
                    estimacion.vd.dependiente=met.est.vd)    
  }
  # FIN SI JERARQUICA  
    
  print(metodo.est)
  cat('',fill=TRUE)

#----------------------------------------------------------------------------
# SE ESTIMAN LOS MODELOS
# MEDIADORA AS VD
  # Si lineal lm
  if(lineal.m & !estima.jerarquica){
    mod.m=lm(eval(parse(text=mod.m)), data=datos)
  }
  # Fin li lineal lm
  
  # Si lineal lmer
  if(lineal.m & estima.jerarquica){
    if(is.null(fac.random[1])){
      f1=paste('(1 | ',random,')',sep='')
    }  
    if(!is.null(fac.random[1])){
    	if(fac.random==mediacion$m)	f1=paste('(1 | ',random,')',sep='')   		
    }     
    if(!is.null(fac.random[1])){
    	if(fac.random != mediacion$m) f1=paste('( ',fac.random,' | ',random,')',sep='')
    }
    mod.m=paste(mod.m,' + ',f1,sep='')
    mod.m=lmer(eval(parse(text=mod.m)), data=datos)
  }  
  # Fin si linear lmer

  # Si no lineal y glm
  if(!lineal.m & !estima.jerarquica){
  	if(logit){
  		mod.m=glm(eval(parse(text=mod.m)), family=binomial(link='logit'),data=datos) 
  	}else{
  		mod.m=glm(eval(parse(text=mod.m)), family=binomial(link='probit'),data=datos) 
  	}
  }
  # Fin no lineal y glm
  
  # Si no lineal y lmer
  if(!lineal.m & estima.jerarquica){
    if(is.null(fac.random[1])){
      f1=paste('(1 | ',random,')',sep='')
    }  
    if(!is.null(fac.random[1])){
    	if(fac.random==mediacion$m)	f1=paste('(1 | ',random,')',sep='')   		
    }     
    if(!is.null(fac.random[1])){
    	if(fac.random != mediacion$m) f1=paste('( ',fac.random,' | ',random,')',sep='')
    }
    mod.m=paste(mod.m,' + ',f1,sep='')
  	if(logit){
  		mod.m=glmer(eval(parse(text=mod.m)), family=binomial(link='logit'),data=datos) 
  	}else{
  		mod.m=glmer(eval(parse(text=mod.m)), family=binomial(link='probit'),data=datos) 
  	}    
  }  
  # Fin no lineal y glmer
 
# VD AS VD 
  # Si lineal 
  if(lineal.vd & !estima.jerarquica){
    mod.vd=lm(eval(parse(text=mod.vd)), data=datos)  
  }
  # Fin si lineal
  # Si lineal y lmer
  if(lineal.vd & estima.jerarquica){
    if(is.null(fac.random[1])){
      f1=paste('(1 | ',random,')',sep='')
    }else{  
      f1=paste('(1 + ',fac.random,' | ',random,')',sep='')
    }  
    cat('-------------------------------------------------------------------',fill=TRUE)
    cat('                      Estimando lmer SE PACIENTE',fill=TRUE)
    cat('-------------------------------------------------------------------',fill=TRUE)    
    mod.vd=paste(mod.vd,' + ',f1,sep='')
    mod.vd=lmer(eval(parse(text=mod.vd)), data=datos)
  }
  # Fin si lineal y lmer

  # Si no lineal y glm
  if(!lineal.vd & !estima.jerarquica){
    if(logit){
  		mod.vd=glm(eval(parse(text=mod.vd)), family=binomial(link='logit'),data=datos) 
  	}else{
  		mod.vd=glm(eval(parse(text=mod.vd)), family=binomial(link='probit'),data=datos) 
  	}
  }
  # Fin si no lineal y glm
  
  # Si no lineal y glmer  
  if(!lineal.vd & estima.jerarquica){
    if(is.null(fac.random[1])){
      f1=paste('(1 | ',random,')',sep='')
    }else{  
      f1=paste('(1 + ',fac.random,' | ',random,')',sep='')
    } 
    mod.vd=paste(mod.vd,' + ',f1,sep='')
    cat('-------------------------------------------------------------------',fill=TRUE)
    cat('                   Estimando glmer SE PACIENTE',fill=TRUE)
    cat('-------------------------------------------------------------------',fill=TRUE)    
  	if(logit){
  		mod.vd=glmer(eval(parse(text=mod.vd)), family=binomial(link='logit'),data=datos) 
  	}else{
  		mod.vd=glmer(eval(parse(text=mod.vd)), family=binomial(link='probit'),data=datos) 
  	}    
  }
  
 
#----------------------------------------------------------------------------
  try(assign('mod.vd',mod.vd,envir=.GlobalEnv),silent=TRUE) 
  try(assign('mod.m',mod.m,envir=.GlobalEnv), silent=TRUE)

  # Estimation via quasi-Bayesian approximation
  cat('-------------------------------------------------------------------',fill=TRUE)
  cat('          GENERANDO Y PROCESANDO 1000 MUESTRAS BOOTSTRAP',fill=TRUE)
  cat('                              SE PACIENTE                       ',fill=TRUE)
  cat('-------------------------------------------------------------------',fill=TRUE)
  cat('',fill=TRUE)
  contcont= mediate(mod.m, mod.vd, sims=1000, treat=mediacion$vi, mediator=mediacion$m)
  med=summary(contcont); e_=contcont
  
  if(sensibilidad){
	sens=medsens(contcont, rho.by = 0.1, effect.type = "indirect")
	print(summary(sens))
	x11()
	print(plot(sens, sens.par = "rho"))
  }	  
  
  # SI MEDIACION MODULADA
  if(med.mod_){
    #Si no se declaran cortes
    if(is.na(med.mod)[[1]]){
      # Si la moduladora es factor
      if(is.factor(datos[,covari]) | is.character(datos[,covari])){
        niveles=names(frecuencias.fnc(datos,covari,silente=TRUE)[[1]]$tabla)
        # Si el factor tiene mas de dos niveles
        if(nlevels(as.factor(datos[,covari])) > 2){
          cat('',fill=TRUE)
          cat('*** Warning. Has incluido como moduladora un factor con mas de dos niveles',fill=TRUE)
          cat('*** Se utilizaran los dos primeros como puntos a contrastar en la modulacion.',fill=TRUE)
          cat('*** Si deseas cualquier otro par debes indicarlo en la misma lista. ',fill=TRUE)
          cat('*** Ej. med.mod=list(zona=c(1,3))',fill=TRUE)
          cat("*** Ej. med.mod=list(zona=c('A','C'))",fill=TRUE)
          cortes=niveles[1:2]
        }else{
          cortes=niveles
        }
        # Fin si dos o mas niveles
        # Si no es factor	
      }else{
        cortes=summary(datos[,covari])[c(2,5)]
      } 
      #Fin si es factor   
    } #Fin si no cortes
    
    # Si cortes
    if(!is.na(med.mod)[[1]]){
      if(is.factor(datos[,covari]) | is.character(datos[,covari])){
        niveles=names(frecuencias.fnc(datos,covari,silente=TRUE)[[1]]$tabla)
        if(is.numeric(med.mod[[1]][1:2])){
          f1=med.mod[[1]][1:2]
          cortes=niveles[f1]
        }else{  
          cortes=med.mod[[1]][1:2]
        }
      }else{
        cortes=med.mod[[1]][1:2]
      }
    } 
    # Fin si cortes
    
    lista1=list();  lista2=list( )
    lista1[[1]]=cortes[1];  names(lista1)=covari
    lista2[[1]]=cortes[2];  names(lista2)=covari
    cat('-------------------------------------------------------------------',fill=TRUE)
    cat('    GENERANDO Y PROCESANDO 3000 MUESTRAS BOOTSTRAP DE MODERACION',fill=TRUE)
    cat('                              SE PACIENTE                       ',fill=TRUE)
    cat('-------------------------------------------------------------------',fill=TRUE)
    cat('',fill=TRUE)            
    
    # Calculamos mediacion para cada valor de covariante
    med.mod1=mediate(mod.m, mod.vd, sims=1000, treat=mediacion$vi, 
    		mediator=mediacion$m, covariates=lista1)
    med.mod2=mediate(mod.m, mod.vd, sims=1000, treat=mediacion$vi, 
    		mediator=mediacion$m, covariates=lista2)
    med.mod1=summary(med.mod1); med.mod2=summary(med.mod2)
    
    test.mod.med=test.modmed(contcont, covariates.1 = lista1, 
                             covariates.2 =lista2, sims = 1000)
    test.mod.med=unclass(test.mod.med)
    lab1=paste('Mediacion para ',covari,' en ',cortes[1],' vs ',cortes[2],sep='')
    lab2=paste('Efecto Directo para ',covari,' en ',cortes[1],' vs ',cortes[2],sep='')
    names(test.mod.med)=c(lab1,lab2)
    lista.med.mod=list(a=med.mod1,b=med.mod2)
    names(lista.med.mod)=c(paste(covari,'.en.',cortes[1],sep=''),paste(covari,'.en.',cortes[2],sep=''))
    lista.med.mod[[3]]=test.mod.med
  }
  # FIN SI MEDIACION MODULADA
  
  if(interac | covari.interac) test.interac=test.TMint(contcont)
  
  res.m=summary(mod.m); res.vd=summary(mod.vd)
  if(lineal.m & !estima.jerarquica){
    res.anova.m=round(
      c(R2=res.m$r.squared, R2.adj=res.m$adj.r.squared, 
        F=as.numeric(res.m$fstatistic[1]), 
        df1=as.numeric(res.m$fstatistic[2]), 
        df2=as.numeric(res.m$fstatistic[3]),
        p.val=as.numeric(1-pf(res.m$fstatistic[1],res.m$fstatistic[2],res.m$fstatistic[3]))),3)
  }
  
  if(lineal.m & estima.jerarquica){
    res.anova.m=Anova(mod.m); r.names=row.names(res.anova.m); n.row=dim(res.anova.m)[1]
    R2=cor(predict(mod.m),datos[,mediacion$m])^2
    res.anova.m[(n.row+2),1]=R2
    r.names=c(r.names,'','R2')
    row.names(res.anova.m)=r.names	   
  }
  if(!lineal.m){
    res.anova.m=Anova(mod.m); r.names=row.names(res.anova.m); n.row=dim(res.anova.m)[1]
    R2=cor(predict(mod.m),datos[,mediacion$m])^2
    res.anova.m[(n.row+2),1]=R2
    r.names=c(r.names,'','R2')
    row.names(res.anova.m)=r.names	  
  }
  
  if(lineal.vd & !estima.jerarquica){
    res.anova.vd=round(
      c(R2=res.vd$r.squared, R2.adj=res.vd$adj.r.squared, 
        F=as.numeric(res.vd$fstatistic[1]), 
        df1=as.numeric(res.vd$fstatistic[2]), 
        df2=as.numeric(res.vd$fstatistic[3]),
        p.val=as.numeric(1-pf(res.vd$fstatistic[1],res.vd$fstatistic[2],res.vd$fstatistic[3]))),3)
  }
  if(lineal.vd & estima.jerarquica){
    res.anova.vd=Anova(mod.vd); r.names=row.names(res.anova.vd); n.row=dim(res.anova.vd)[1]
    R2=cor(predict(mod.vd),datos[,mediacion$vd])^2
    res.anova.vd[(n.row+2),1]=R2
    r.names=c(r.names,'','R2')
    row.names(res.anova.vd)=r.names	
  }
  
  if(!lineal.m){
    res.anova.vd=Anova(mod.vd); r.names=row.names(res.anova.vd); n.row=dim(res.anova.vd)[1]
    R2=cor(predict(mod.vd),datos[,mediacion$vd])^2
    res.anova.vd[(n.row+2),1]=R2
    r.names=c(r.names,'','R2')
    row.names(res.anova.vd)=r.names
  }
  
  if(lineal.m & !lineal.vd & !estima.jerarquica){
    res.anova.vd=Anova(mod.vd); r.names=row.names(res.anova.vd); n.row=dim(res.anova.vd)[1]
    R2=cor(predict(mod.vd),datos[,mediacion$vd])^2
    res.anova.vd[(n.row+2),1]=R2
    r.names=c(r.names,'','R2')
    row.names(res.anova.vd)=r.names
  }  
  
  modelo.m=list(vd=mediacion$m, Coef.=round(res.m$coeff,4), Anova=res.anova.m) 
  modelo.vd=list(vd=mediacion$vd, Coef.=round(res.vd$coeff,4), Anova=res.anova.vd) 
  
  res=list(Mediadora.dependiente=modelo.m, Modelo.vd=modelo.vd,
           Mediacion.Average.Causal.Mediation.Effects=summary(contcont))
  
  if(interac | covari.interac){
    inte=paste('Interaccion.',mediacion$vi,':',mediacion$m,sep='')
    res=list(Mediadora.dependiente=modelo.m, Modelo.vd=modelo.vd, 
             Mediacion.Average.Causal.Mediation.Effects=summary(contcont), Interaccion=test.interac)
    names(res)[4]=inte  
  }
  if(med.mod_){
    res=list(Mediadora.dependiente=modelo.m, Modelo.vd=modelo.vd, 
             Mediacion.Average.Causal.Mediation.Effects=summary(contcont), 
             Mediacion.Modulada=lista.med.mod)	
  }
  x11()	  
  plot (0,0,type='l',lwd=2,xlim=c(0.5,12.5) ,ylim=c(-1,7),xlab='',ylab='',
        frame.plot=FALSE, yaxt='n', xaxt='n', main='ANALISIS DE MEDIACION')
  segments(1,1,4,1); segments(1,1,1,2.5); segments(1,2.5,4,2.5); segments(4,1,4,2.5)
  segments(9,1,12,1); segments(9,1,9,2.5); segments(9,2.5,12,2.5); segments(12,1,12,2.5)
  segments(5,4,8,4); segments(5,4,5,5.5); segments(5,5.5,8,5.5); segments(8,4,8,5.5)
  arrows(4.2,1.75,8.7,1.75, code=2);	arrows(2.5,2.7,4.7,4.7, code=2)
  arrows(8.2,4.7,10.5,2.7, code=2)
  text(2.5,1.8,label='VI'); text(6.5,4.7, label='M'); text(10.5,1.8,label='VD')
  box()
  c1=data.frame(res.vd$coeff); b1=data.frame(res.m$coeff)
  m.vi=b1[mediacion$vi,1]; vd.vi=c1[mediacion$vi,1]; vd.m=c1[mediacion$m,1]
  text(6.2,1.9,label=round(vd.vi,4) ); #-0.048
  text(3,3.8,label=round(m.vi,4));    #0.0674
  text(10,3.8,label=round(vd.m,4));   #-0.22
  text(3,0.5, label=paste('ACME ',round(e_$d0,3),sep='='))
  text(5.3,0.5, label=paste('(p= ',round(e_$d0.p,5),')',sep=''))
  text(2.84,0, label=paste('ADE ',round(e_$z0,3),sep='='))
  text(5,0, label=paste('(p= ',round(e_$z0.p,5),')',sep=''))
  text(3.35,-0.5, label=paste('Efecto total= ',round(e_$tau.coef,3),sep=''))
  text(6,-0.5, label=paste('(p= ',round(e_$tau.p,5),')',sep=''))
  x11(); plot(contcont)	  
  try(detach(package:mediation),silent=TRUE)
  if(estima.jerarquica) try(detach(package:lme4),silent=TRUE)
  try(rm(mod.m,envir=.GlobalEnv),silent=TRUE) 
  try(rm(mod.vd,envir=.GlobalEnv),silent=TRUE) 
  return(res)
}
# -------------------------------------------------------------------------------
check.variables.fnc=function(datos,variables){
  nombres=names(datos)
  indice=variables %in% nombres
  if(sum(indice) < length(variables)){
    indice=!indice; chivato=variables[indice]
    cat('',fill=TRUE)
    cat('*** Error. La variable ',chivato,' no existe en la base de datos ***',fill=TRUE)
    cat('',fill=TRUE)
    stop( )
  }  
}   
# -------------------------------------------------------------------------------
  
# -------------------------------------------------------------------------------
# baremo.fnc by Pedro Prieto. 
# A partir de un vector numerico calcula puntuaciones tipicas, percentiles, 
# puntuaciones T, D y eneatipos
# -------------------------------------------------------------------------------
 baremo.fnc=function(X){
  crea.cat.fnc('Calculo de baremos (by Pedro Prieto)')
  
  X=na.omit(X)
  if(class(X)!='numeric' & class(X)!= 'integer'){
    cat('', fill=TRUE)
    cat('*** Error. Debes introducir una variable numerica como unico argumento ***',fill=TRUE)
    cat('', fill=TRUE)
    stop( )
  }
  tabla <- table(X)
  #percentiles hasta puntos medios
  nc=length(tabla)
  per=rep(0,nc)
  per[1]=(tabla[1]/2)/length(X)
  for (i in 2:(nc)){
    s1=cumsum(tabla)[i-1]
    s1
    s2=tabla[i]/2
    per[i]=(s1+s2)/length(X)
  }
  #descriptivos.fnc(datos)
  nsuj=length(X)
  media=mean(X)
  media
  varianza=var(X*(nsuj-1)/nsuj)
  dvt=sqrt(varianza)
  dvt
  valores=as.numeric(names(tabla))
  z=(valores-media)/dvt
  # Puntuaciones T
  T = round(50 + (10 * z), digits=0)
  # Puntuaciones D
  D = round(50 + (20 * z), digits=0)
  # Eneatipos (asumiendo distribucion normal de las puntuaciones)
  #datosb$E =round(5 + (2 * datosb$z.V1), digits=0)
  z.norm=qnorm(per)
  E =round(5 + (2 * z.norm), digits=0)
  for (i in 1:length(E)){
    if (E[i] <= 0){E[i]=1}
    if (E[i] >= 10){E[i]=9}
  }
  baremo=as.data.frame(cbind(valores,100*round(per,2),round(z,2),T,D,round(z.norm,2),E))
  names(baremo)=c('X','percentil','z','T','D','z.norm','En.')
  return(baremo)
}
# -------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
# grafica.likert.fnc(pisaitems, variables=2:12)
# -------------------------------------------------------------------------------
 grafica.likert.fnc=function(datos, variables=NULL, var.label=NULL, etiquetas=NULL, 
						to.pdf=FALSE, ordenado=NULL, que.factor=NULL, titulo=NULL){
	nombres=names(datos)
	# CHECK VARIABLES
	if(!is.null(variables)){
		if(is.numeric(variables[1])){
			variables=nombres[variables]
			if(sum(is.na(variables))!=0){
				cat('',fill=TRUE)
				cat('*** Error. Has indicado un numero incorrecto de variables.    ***',fill=TRUE)
				cat('*** Revisa los valores introducidos en el argumento variables ***',fill=TRUE)
				cat('',fill=TRUE)
				stop()
			}
		}	
	}else{
		variables=nombres
	}		
	# FIN CHECK VARIABLES

	# SI HAY VAR.LABEL			
	if(!is.null(var.label)){
		if(!is.list(var.label)) var.label=as.list(var.label)
		if(length(var.label)!=dim(datos[,variables])[2]){
			cat('',fill=TRUE)
			cat('*** Error. El numero de etiquetas de las variables es diferente     ***',fill=TRUE)
			cat('*** al de las variables que has incluido en el argumento variables. ***',fill=TRUE)
			cat('',fill=TRUE)
			stop()
		}					
		
		indice=names(var.label) %in% nombres
		
		if(sum(indice) !=length(variables)){
			vari.error=nombres[!indice]
			cat('',fill=TRUE)
			cat('*** Error. Has indicado algun o algunos nombres de variables en ***',fill=TRUE)
			cat('*** el argumento var.label que no existen en la base de datos.  ***',fill=TRUE)
			pegar=paste('*** La variable de var.label: ',vari.error,' no existe',sep='')
			cat(pegar,fill=TRUE)
			cat('',fill=TRUE)
			stop()
		}							
	
		# CAMBIAMOS EL NOMBRE DE LAS VARIABLES POR SUS ETIQUETAS
		for(i in 1:length(var.label)) 
			datos=cambia.nombre.var.fnc(datos, names(var.label)[i], var.label[[i]], silente=TRUE)	
		variables=as.character(do.call(rbind,var.label)[,1])	
	}	
	# FIN SI VAR.LAB	
	
	#------------------------------------------------------------
	# PROC SI NO HAY FACTOR
	if(is.null(que.factor[1])){
		require(sjPlot, quietly=TRUE)
		dat_=datos[,variables]
		if(!is.null(etiquetas)){
			etiquetas=list(etiquetas)
		}
		# Si etiqueta is NULL
		if(is.null(etiquetas)){
			n.var=dim(dat_)[2]
			lista=list()
			for(i in 1:n.var) lista[[i]]=is.factor(dat_[,i])
			lista=do.call(rbind,lista)[,1]
			if(sum(lista)==n.var) etiquetas=levels(dat_[,1])
			if(sum(lista) > 0 & sum(lista) < n.var){
				cat('',fill=TRUE)
				cat('*** Error. Entre las variables incluidas hay algunos factores  y otros ***',fill=TRUE)
				cat('*** tipos de variables. Todas las variables incluidas deben ser del    ***',fill=TRUE)
				cat('*** mismo tipo y con los mismos niveles.                               ***',fill=TRUE)
				cat('',fill=TRUE)
				stop()			
			}
			lista=list()
			for(i in 1:n.var) lista[[i]]=is.numeric(dat_[,i]) | is.integer(dat_[,i])
			lista=do.call(rbind,lista)[,1]			
			if(sum(lista)==n.var){
				etiquetas=levels(as.factor(unique(dat_[,1])))
				cat('',fill=TRUE)
				cat('*** WARNING. No has incluido el argumento etiquetas (etiquetas de los valores  ***',fill=TRUE)
				cat('*** de las variables. Se utilizaran como etiquetas los valores numericos.      ***',fill=TRUE)
				cat('*** Puedes incluir ese argumento con las etiquetas que deseas para los valores ***',fill=TRUE)
				cat("*** Ej: etiquetas=c('Tot en desacuerdo','De acuerdo','Tot de acuerdo')         ***",fill=TRUE)
				cat('',fill=TRUE)
			}
		}	
		# Fin si es null etiquetas
		
		if(is.factor(dat_[,1])) 
			for(i in 1:dim(dat_)[2]) dat_[,i]=as.numeric(dat_[,i])
				
		if(!to.pdf) x11()
		sjp.likert(dat_, legendLabels=etiquetas, barColor="brown", valueLabelSize=3,
			axisLabels.y=names(dat_), orderBy=ordenado, title=titulo,axisLabelSize=0.8,
			legendPos='bottom', legendSize = 0.8)
		
		try(detach(package:sjPlot),silent=TRUE)
	}
	# FIN SI NO HAY FACTOR
	
	#------------------------------------------------------------
	# PROC SI HAY QUE.FACTOR
	if(!is.null(que.factor)){
		datos=datos[,c(que.factor,variables)]
		
		# CHECK SI LAS VARIABLES SON FACTORES
		n.var=length(variables)
		lista=list()
		for(i in 2:(n.var+1)) lista[[i]]=is.factor(datos[,i])
		lista=do.call(rbind,lista)[,1]
		
		# SI LAS VARIABLES SON NUMERICAS
		if(sum(!lista)==n.var){	
			for(i in 2:dim(datos)[2]) datos[,i]=as.factor(datos[,i])
			niveles=levels(datos[,2])

			# Si no se incluyen etiquetas
			if(is.null(etiquetas)){
				cat('',fill=TRUE)
				cat('*** WARNING. No has incluido el argumento etiquetas (etiquetas de los valores  ***',fill=TRUE)
				cat('*** de las variables. Se utilizaran como etiquetas los valores numericos.      ***',fill=TRUE)
				cat('*** Puedes incluir ese argumento con las etiquetas que deseas para los valores ***',fill=TRUE)
				cat("*** Ej: etiquetas=c('Tot en desacuerdo','De acuerdo','Tot de acuerdo')         ***",fill=TRUE)
				cat('',fill=TRUE)
			}else{	
				if(length(etiquetas) != length(niveles)){
					cat('',fill=TRUE)
					cat('*** Error. El numero de etiquetas incluidas no coindice con el numero ***',fill=TRUE)
					cat('*** de valores diferentes de las variables.                           ***',fill=TRUE)
					cat('',fill=TRUE)
					stop()			
				}
				# Recodifica segun las etiquetas
				require(car, quietly=TRUE)
				for(i in 2:dim(datos)[2]){
					for(j in 1:length(etiquetas)){
						antiguo=niveles[j]; nuevo=etiquetas[j]
						try(assign('antiguo',antiguo,envir=.GlobalEnv),silent=TRUE) 
						try(assign('nuevo',nuevo,envir=.GlobalEnv),silent=TRUE) 
						datos[,i]=recode(datos[,i], " antiguo = nuevo")
					}
					datos=reordena.factor.fnc(datos, que.factor=names(datos)[i], 
						niveles=etiquetas, silente=TRUE)
				}	
				#try(detach(package:car),silent=TRUE)
			}
			# FIN SI O NO ETIQUETAS
		}	
		dat_=datos[,variables]
		require(likert, quietly=TRUE)
	
		f1=likert(dat_)
		x.factor=divide.por.factor.fnc(datos, que.factor=que.factor, silente=TRUE)
		lista=list()
		for(i in 1:length(x.factor)) lista[[i]]=likert(x.factor[[i]][,-1])$results
 		names(lista)=names(x.factor)
		nombres=names(lista)
		for(i in 1:length(lista)){
			x=lista[[i]]
			n.var=dim(x)[2]
			x$Group=nombres[i]
			x=x[,c(n.var+1,1:n.var)]
			lista[[i]]=x
		}
		lista=data.frame(do.call(rbind,lista))
		row.names(lista)=1:dim(lista)[1]
		f1$results=lista
		f1$grouping=que.factor
		if(!to.pdf) x11()
		print(likert.bar.plot(f1, legend = "Respuesta",))
		
		try(detach(package:likert),silent=TRUE)
	}
	# FIN SI QUE FACTOR
 }
# -------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
# recode function (J. Fox)
# last modified 2012-09-30 by J. Fox
# -------------------------------------------------------------------------------
recode <- function(var, recodes, as.factor.result, as.numeric.result=TRUE, levels){
	recodes <- gsub("\n|\t", " ", recodes)
	recode.list <- rev(strsplit(recodes, ";")[[1]])
	is.fac <- is.factor(var)
	if (missing(as.factor.result)) as.factor.result <- is.fac
	if (is.fac) var <- as.character(var)
	result <- var
	if (is.numeric(var)) {
		lo <- min(var, na.rm=TRUE)
		hi <- max(var, na.rm=TRUE)
	}
	for (term in recode.list){
		if (0 < length(grep(":", term))) {
			range <- strsplit(strsplit(term, "=")[[1]][1],":")
			low <- eval(parse(text=range[[1]][1]))
			high <- eval(parse(text=range[[1]][2]))
			target <- eval(parse(text=strsplit(term, "=")[[1]][2]))
			result[(var >= low) & (var <= high)] <- target
		}
		else if (0 < length(grep("^else=", squeezeBlanks(term)))) {
			target <- eval(parse(text=strsplit(term, "=")[[1]][2]))
			result[1:length(var)] <- target
		}
		else {
			set <- eval(parse(text=strsplit(term, "=")[[1]][1]))
			target <- eval(parse(text=strsplit(term, "=")[[1]][2]))
			for (val in set){
				if (is.na(val)) result[is.na(var)] <- target
				else result[var == val] <- target
			}
		}
	}
	if (as.factor.result) {
		result <- if (!missing(levels)) factor(result, levels=levels) 
			else as.factor(result)
	}
	else if (as.numeric.result && (!is.numeric(result))) {
		result.valid <- na.omit(result)
		opt <- options("warn"=-1)
		result.valid <- as.numeric(result.valid)
		options(opt)
		if (!any(is.na(result.valid))) result <- as.numeric(result)
	}
	result
}

squeezeBlanks <- function(text){
  gsub(" *", "",  text)
}
# -------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
# FUNCION EXTRAIDA LA LA LIBRERIA sjPlot Y ADAPTADA PARA PERMITIR
# LAS INTERACCIONES CON REGRESION LOGISTICA
# http://strengejacke.wordpress.com/sjplot-r-package/
# Author Daniel Ludecke <d.luedecke@uke.de>
# -------------------------------------------------------------------------------
sjp.lm.int_ <- function(fit, is.log=FALSE, variables,br=br,
                       smooth="none",
                       diff=FALSE,
                       swapPredictors=TRUE,
                       title=NULL,
                       titleSize=1.3,
                       titleColor="black",
                       fillColor="grey",
                       fillAlpha=0.4,
                       lowerBoundColor="#3366cc",
                       upperBoundColor="#cc3300",
                       lineColor="#33cc66",
                       axisTitle.x=NULL,
                       axisTitle.y=NULL,
                       axisLabelColor="gray30", 
                       axisLabelSize=1.1,
                       axisTitleColor="black",
                       axisTitleSize=1.3,
                       legendLabels=NULL,
                       legendLabelSize=0.9,
                       legendLabelColor="black",
                       showValueLabels=FALSE,
                       valueLabelSize=4,
                       valueLabelColor="black",
                       valueLabelAlpha=0.8,
                       breakTitleAt=50,
                       breakLegendLabelsAt=20,
                       gridBreaksAt=NULL,
                       theme=NULL,
                       showTickMarks=TRUE,
                       borderColor=NULL, 
                       axisColor=NULL, 
                       majorGridColor=NULL,
                       minorGridColor=NULL,
                       hideGrid.x=FALSE,
                       hideGrid.y=FALSE) {
                       
  require(ggplot2, quietly=TRUE)                      
  # -----------------------------------------------------------
  # parameter check
  # -----------------------------------------------------------
  if (is.null(fillColor)) {
    fillColor="white"
    fillAlpha=0
  }
  if (is.null(gridBreaksAt)) {
    gridbreaks.x <- gridbreaks.y <- waiver()
  }
  # -----------------------------------------------------------
  # retrieve amount of predictor variables
  # -----------------------------------------------------------
  listpv <- attr(fit$terms,"predvars")
  predvars <- c()
  # -----------------------------------------------------------
  # remove first two elements (including dependent variable)
  # -----------------------------------------------------------
  for (i in 3:length(listpv)) {
    predvars <- c(predvars, listpv[[i]])
  }
  # remember length of predictor variables
  predvars.length <- length(predvars)
  # -----------------------------------------------------------
  # retrieve p-values, without intercept
  # -----------------------------------------------------------
  pval <- summary(fit)$coefficients[-1,4]
  # -----------------------------------------------------------
  # retrieve estimates, without intercept
  # -----------------------------------------------------------
  estimates <- summary(fit)$coefficients[-1,1]
  estimates.names <- names(estimates)
  # -----------------------------------------------------------
  # retrieve estimate of intercept
  # -----------------------------------------------------------
  b0 <- estimates.intercept <- summary(fit)$coefficients[1,1]
  
  
  # -----------------------------------------------------------
  # find all significant interactions
  # we start looking for significant p-values beginning
  # with the first interaction, not the first single term!
  # thus, the starting point is first position after all single
  # predictor variables
  # -----------------------------------------------------------
  # save names of interaction predictor variables into this object
  intnames <- c()
  for (i in (predvars.length+1):length(pval)) {
    if (pval[i] < 0.05) {
      intnames <- c(intnames, names(pval[i]))
    }
  }
  # check for any signigicant interactions, stop if nothing found
  if (is.null(intnames)) {
    return('no.interaccion')
    stop("Ningua interaccion ha resultado significativa ...", call.=FALSE)
  }
  # check if interaction between factors
  interac=intnames[do.call(rbind,lapply(strsplit(intnames, ":") , function(x) length(x)==2))[,1]]
  interac=do.call(rbind,strsplit(interac, ":"))
  interac_=data.frame(matrix(interac %in% variables$variables,ncol=2))
  interac_$suma=apply(interac_,1,sum); 
  interac_$interac=1:dim(interac_)[1]
  que.interac=subset(interac_, suma==2)
  if(dim(que.interac)[1] ==0){
    cat('',fill=TRUE)
    cat('*** WARNING. Tienes interacciones solo de variables cualitativas.', fill=TRUE)
    cat('*** La grafica de interaccion se realiza exclusivamente para interacciones', fill=TRUE)
    cat('*** dobles de ambas variables cuantitativas.',fill=TRUE)
    cat('',fill=TRUE)
    return('solo.variables.cualitativas')
    stop( )
  }
  if(dim(que.interac)[1] < dim(interac)[1]){
    cat('*** WARNING. Solo se grafican las interacciones entre variables cuantitativas',fill=TRUE)
  }
  interac=interac[que.interac$interac,]
  intnames=as.character(paste(interac[,1],interac[,2],sep=':'))
  
  # --------------------------------------------------------
  # Set theme and default grid colours. grid colours
  # might be adjusted later
  # --------------------------------------------------------
  hideGridColor <- c("white")
  if (is.null(theme)) {
    ggtheme <- theme_gray()
    hideGridColor <- c("gray90")
  }else if (theme=="bw"){
    ggtheme <- theme_bw()
  }else if (theme=="classic") {
    ggtheme <- theme_classic()
  }else if (theme=="minimal") {
    ggtheme <- theme_minimal()
  }else if (theme=="none") {
    ggtheme <- theme_minimal()
    majorGridColor <- c("white")
    minorGridColor <- c("white")
    showTickMarks <-FALSE
  }
  # --------------------------------------------------------
  # Hide or show Tick Marks
  # --------------------------------------------------------
  if (!showTickMarks) {
    ggtheme <- ggtheme + theme(axis.ticks = element_blank())
  }
  # --------------------------------------------------------
  # Set up grid colours
  # --------------------------------------------------------
  majorgrid <- NULL
  minorgrid <- NULL
  if (!is.null(majorGridColor)) {
    majorgrid <- element_line(colour=majorGridColor)
  }
  if (!is.null(minorGridColor)) {
    minorgrid <- element_line(colour=minorGridColor)
  }
  hidegrid <- element_line(colour=hideGridColor)
  
  
  # -----------------------------------------------------------
  # copy variable values to data frame
  # -----------------------------------------------------------
  if(is.log & br) fitdat=fit$data[,-1]
  if(is.log & !br) fitdat <- as.data.frame(fit$x)
  # -----------------------------------------------------------
  # Now iterate all significant interaction terms
  # and manually calculate the linear regression by inserting
  # the estimates of each term and the associated interaction term,
  # i.e.: y = b0 + (b1 * pred1) + (b2 * pred2) + (b3 * pred1 * pred2)
  # -----------------------------------------------------------
  n.interac=length(intnames)
  for (cnt in 1:length(intnames)) {
    # -----------------------------------------------------------
    # first, retrieve and split interaction term so we know 
    # the two predictor variables of the interaction term
    # -----------------------------------------------------------
    interactionterms <- strsplit(intnames[cnt], ":")
    labx <- c()
    # Label on y-axis is name of dependent variable
    laby <- listpv[[2]]
    # -----------------------------------------------------------
    # find estimates (beta values) for each single predictor of
    # the interaction as well as of the interaction term
    # -----------------------------------------------------------
    b1 <- as.numeric(estimates[match(interactionterms[[1]][1], estimates.names)])
    b2 <- as.numeric(estimates[match(interactionterms[[1]][2], estimates.names)])
    b3 <- as.numeric(estimates[match(intnames[cnt], estimates.names)])
    # -----------------------------------------------------------
    # retrieve number of unique values in each predictor variable.
    # depending on the amount of values the variable for the x-axis
    # is chosen. In this case, we use the predictor with the higher
    # number of unique values on the x-axis.
    # -----------------------------------------------------------
    # retrieve values as data frame
    df_pred1uniquevals <- unique(fitdat[interactionterms[[1]][1]])
    df_pred2uniquevals <- unique(fitdat[interactionterms[[1]][2]])
    # convert data frame to numeric vector
    pred1uniquevals <- pred2uniquevals <- as.numeric(c())
    pred1uniquevals <- sort(as.numeric(c(apply(df_pred1uniquevals, c(1), as.numeric ))))
    pred2uniquevals <- sort(as.numeric(c(apply(df_pred2uniquevals, c(1), as.numeric ))))
    # init data frame
    intdf <- c()
    # -----------------------------------------------------------
    # choose x-value according to higher number of unique values
    # choose minimum and maximum value from predictor that has
    # a "smaller range" (i.e. less unique values)
    # or swap predictors on axes if requested
    # -----------------------------------------------------------
    if (swapPredictors) {
      useFirstPredOnY <- ifelse(length(pred1uniquevals) > length(pred2uniquevals), F, T)
    }else{
      useFirstPredOnY <- ifelse(length(pred1uniquevals) > length(pred2uniquevals), T, F)
    }
    # -----------------------------------------------------------
    # calculate regression line
    # -----------------------------------------------------------
    if (useFirstPredOnY) {
      labx <- c(interactionterms[[1]][1])
      predy <- c(interactionterms[[1]][2])
      ymin <- min(pred2uniquevals)
      ymax <- max(pred2uniquevals)
      # -----------------------------------------------------------
      # Create data frame for plotting the interactions by
      # manually calculating the linear regression by inserting
      # the estimates of each term and the associated interaction term,
      # i.e.: y = b0 + (b1 * pred1) + (b2 * pred2) + (b3 * pred1 * pred2)
      # -----------------------------------------------------------
      for (j in 1:length(pred1uniquevals)) {
        # iterate x-values and calculate minimum y
        pr <- pred1uniquevals[j]
        # ------------------------------
        # We now calculate the effect of predictor 1 under absence (or lowest
        # impact) of predictor 2 on the dependent variable. Thus, the slope for
        # predictor 2 is not calculated. see
        # http://www.theanalysisfactor.com/interpreting-interactions-in-regression/
        # http://www.theanalysisfactor.com/clarifications-on-interpreting-interactions-in-regression/
        # ------------------------------
        # miny = (b0 + (b1*pr) + (b2*ymin) + (b3*pr*ymin))
        if(is.log){
			miny =1/(1+exp(-(b0 + (b1*pr) + (b3*pr*ymin))))
		}else{
			miny =(b0 + (b1*pr) + (b3*pr*ymin))
		}
        # ------------------------------
        # here we calculate the effect of predictor 1 under presence (or strongest
        # impact) of predictor 2 on the dependent variable. Thus, the slope for
        # predictor 2 only is not needed. see references above
        # ------------------------------
        # maxy = (b0 + (b1*pr) + (b2*ymax) + (b3*pr*ymax))
        if(is.log){
			maxy =1/(1+exp(-(b0 + (b1*pr) + (b3*pr*ymax))))
		}else{
			maxy =(b0 + (b1*pr) + (b3*pr*ymax))
		}	
        # store in df
        tmp <- as.data.frame(cbind(x=j, y=miny, ymin=miny, ymax=maxy, grp="min"))
        intdf <- as.data.frame(rbind(intdf, tmp))
        # store in df
        tmp <- as.data.frame(cbind(x=j, y=maxy, ymin=miny, ymax=maxy, grp="max"))
        intdf <- as.data.frame(rbind(intdf, tmp))
      }
    }else {
      labx <- c(interactionterms[[1]][2])
      predy <- c(interactionterms[[1]][1])
      ymin <- min(pred1uniquevals)
      ymax <- max(pred1uniquevals)
      # -----------------------------------------------------------
      # Create data frame for plotting the interactions by
      # manually calculating the linear regression by inserting
      # the estimates of each term and the associated interaction term,
      # i.e.: y = b0 + (b1 * pred1) + (b2 * pred2) + (b3 * pred1 * pred2)
      # -----------------------------------------------------------
      # compute for minimum value
      for (j in 1:length(pred2uniquevals)) {
        # iterate x-values and calculate minimum y
        pr <- pred2uniquevals[j]
        # ------------------------------
        # We now calculate the effect of predictor 2 under absence (or lowest
        # impact) of predictor 1 on the dependent variable. Thus, the slope for
        # predictor 1 is not calculated. see
        # http://www.theanalysisfactor.com/interpreting-interactions-in-regression/
        # http://www.theanalysisfactor.com/clarifications-on-interpreting-interactions-in-regression/
        # ------------------------------
        # miny = (b0 + (b1*ymin) + (b2*pr) + (b3*pr*ymin))
        if(is.log){
			miny =1/(1+exp(-(b0 + (b2*pr) + (b3*pr*ymin))))
		}else{
			miny =(b0 + (b2*pr) + (b3*pr*ymin))
		}
        # ------------------------------
        # here we calculate the effect of predictor 2 under presence (or strongest
        # impact) of predictor 1 on the dependent variable. Thus, the slope for
        # predictor 1 only is not needed. see references above
        # ------------------------------
        # maxy = (b0 + (b1*ymax) + (b2*pr) + (b3*pr*ymax))
        if(is.log){ 
			maxy =1/(1+exp(-(b0 + (b2*pr) + (b3*pr*ymax))))
		}else{
			maxy =(b0 + (b2*pr) + (b3*pr*ymax))
		}
        # store in df
        tmp <- as.data.frame(cbind(x=j, y=miny, ymin=miny, ymax=maxy, grp="min"))
        intdf <- as.data.frame(rbind(intdf, tmp))
        # store in df
        tmp <- as.data.frame(cbind(x=j, y=maxy, ymin=miny, ymax=maxy, grp="max"))
        intdf <- as.data.frame(rbind(intdf, tmp))
      }
    }
    # -----------------------------------------------------------
    # convert df-values to numeric
    # -----------------------------------------------------------
    intdf$x <- as.numeric(as.character(intdf$x))
    intdf$y <- as.numeric(as.character(intdf$y))
    intdf$ymin <- as.numeric(as.character(intdf$ymin))
    intdf$ymax <- as.numeric(as.character(intdf$ymax))
    intdf$ydiff <- intdf$ymax-intdf$ymin
    # -----------------------------------------------------------
    # retrieve lowest and highest x and y position to determine
    # the scale limits
    # -----------------------------------------------------------
    lowerLim.x <- floor(min(intdf$x))
    upperLim.x <- ceiling(max(intdf$x))
    lowerLim.y <- floor(min(intdf$y))
    upperLim.y <- ceiling(max(intdf$y))
    # -----------------------------------------------------------
    # check whether user defined grid breaks / tick marks are used
    # -----------------------------------------------------------
    if (!is.null(gridBreaksAt)) {
      gridbreaks.x <- c(seq(lowerLim.x, upperLim.x, by=gridBreaksAt))
      gridbreaks.y <- c(seq(lowerLim.y, upperLim.y, by=gridBreaksAt))
    }
    # -----------------------------------------------------------
    # prepare plot title and axis titles
    # -----------------------------------------------------------
    if (is.null(title)) {
      #       labtitle <- paste0("Effect of ", interactionterms[[1]][ifelse(useFirstPredOnY==TRUE,1,2)], 
      #                          " on ", listpv[[2]], 
      #                          " under minimum and maximum interaction with ", interactionterms[[1]][ifelse(useFirstPredOnY==TRUE,2,1)])
      labtitle <- paste0("Interaccion de ", interactionterms[[1]][ifelse(useFirstPredOnY==TRUE,1,2)], 
                         " y ", interactionterms[[1]][ifelse(useFirstPredOnY==TRUE,2,1)],
                         " sobre ", listpv[[2]])
    }else {
      labtitle <- title
    }
    if (is.null(legendLabels)) {
      lLabels <- c(paste0("En el minimo de ", predy), paste0("En el maximo de ", predy))
    }else {
      lLabels <- legendLabels
    }
    if (!is.null(axisTitle.x)) {
      labx <- axisTitle.x
    }
    if (!is.null(axisTitle.y)) {
      laby <- axisTitle.y
    }
    # wrap title
    pattern <- c(paste('(.{1,', breakTitleAt, '})(\\s|$)', sep=""))
    for (n in 1:length(labtitle)) {
      labtitle[n] <- gsub(pattern, '\\1\n', labtitle[n])
    }
    # wrap legend labels
    pattern <- c(paste('(.{1,', breakLegendLabelsAt, '})(\\s|$)', sep=""))
    for (n in 1:length(lLabels)) {
      lLabels[n] <- gsub(pattern, '\\1\n', lLabels[n])
    }
    
    # -----------------------------------------------------------
    # prepare base plot of interactions
    # -----------------------------------------------------------
    if (smooth=="none") {
      if (diff) {
        baseplot <- ggplot(intdf, aes(x=x, y=ydiff)) + 
          # add a shaded region between minimun and maximum curve of interactions
          geom_ribbon(aes(x=x, ymin=0, ymax=ydiff), fill=fillColor, alpha=fillAlpha) +
          geom_line(colour=lineColor)
        if (showValueLabels) {
          baseplot <- baseplot +
            geom_text(aes(label=round(ydiff,1), x=x, y=ydiff), colour=valueLabelColor, vjust=1.5, size=valueLabelSize, alpha=valueLabelAlpha, show_guide=FALSE)
        }
      }else{
        baseplot <- ggplot(intdf) + 
          geom_point(aes(x=x, y=y, colour=grp)) +
          # add a shaded region between minimun and maximum curve of interactions
          geom_ribbon(aes(x=x, ymin=ymin, ymax=ymax), fill=fillColor, alpha=fillAlpha) +
          geom_line(aes(x=x, y=y, colour=grp))
        if (showValueLabels) {
          baseplot <- baseplot +
            geom_text(aes(label=round(y,1), x=x, y=y), colour=valueLabelColor, vjust=1.5, size=valueLabelSize, alpha=valueLabelAlpha, show_guide=FALSE)
        }
      }
    }else{
      if (diff) {
        baseplot <- ggplot(intdf, aes(x=x, y=ydiff)) + 
          stat_smooth(colour=lineColor, method=smooth, se=FALSE)
        # ------------------------------------------------------------
        # Thanks to Stackoverflow user Henrik for the following solution
        # (http://stackoverflow.com/q/19643234/2094622)
        # ------------------------------------------------------------
        # build plot object for rendering 
        ggloess <- ggplot_build(baseplot)
        # extract data for the loess lines from the 'data' slot
        loessdf <- data.frame(x = ggloess$data[[1]]$x, ydiff = ggloess$data[[1]]$y)
        # use the loess data to add the 'ribbon' to plot
        # add a shaded region between minimun and maximum curve of interactions
        baseplot <- baseplot + geom_ribbon(data = loessdf, aes(x = x, ymin = 0, ymax = ydiff), fill = fillColor, alpha = fillAlpha)
        if (showValueLabels) {
          baseplot <- baseplot +
            geom_text(data = loessdf, aes(label=round(ydiff,1), x=x, y=ydiff), colour=valueLabelColor, vjust=1.5, size=valueLabelSize, alpha=valueLabelAlpha, show_guide=FALSE)
        }
      }else{
        baseplot <- ggplot(intdf) + 
          stat_smooth(aes(x=x, y=ymin, colour="max"), method=smooth, se=FALSE) +
          stat_smooth(aes(x=x, y=ymax, colour="min"), method=smooth, se=FALSE)
        # ------------------------------------------------------------
        # Thanks to Stackoverflow user Henrik for the following solution
        # (http://stackoverflow.com/q/19643234/2094622)
        # ------------------------------------------------------------
        # build plot object for rendering 
        ggloess <- ggplot_build(baseplot)
        # extract data for the loess lines from the 'data' slot
        loessdf <- data.frame(x = ggloess$data[[1]]$x, ymin = ggloess$data[[1]]$y, ymax = ggloess$data[[2]]$y)
        # use the loess data to add the 'ribbon' to plot
        # add a shaded region between minimun and maximum curve of interactions
        baseplot <- baseplot + geom_ribbon(data = loessdf, aes(x = x, ymin = ymin, ymax = ymax), fill = fillColor, alpha = fillAlpha)
        if (showValueLabels) {
          baseplot <- baseplot +
            geom_text(data = loessdf, aes(label=round(ymin,1), x=x, y=ymin), colour=valueLabelColor, vjust=1.5, size=valueLabelSize, alpha=valueLabelAlpha, show_guide=FALSE) +
            geom_text(data = loessdf, aes(label=round(ymax,1), x=x, y=ymax), colour=valueLabelColor, vjust=1.5, size=valueLabelSize, alpha=valueLabelAlpha, show_guide=FALSE)
        }
      }
    }
    # ------------------------------------------------------------------------------------
    # build plot object with theme and labels
    # ------------------------------------------------------------------------------------
    baseplot <- baseplot + 
      # set plot and axis titles
      labs(title=labtitle, x=labx, y=laby) +
      # set axis scale breaks
      scale_x_continuous(limits=c(lowerLim.x, upperLim.x), breaks=gridbreaks.x) +
      scale_y_continuous(limits=c(lowerLim.y, upperLim.y), breaks=gridbreaks.y) +
      # apply theme
      ggtheme  + 
      # do minor modifications to theme
      theme(axis.text = element_text(size=rel(axisLabelSize), colour=axisLabelColor), 
            axis.title = element_text(size=rel(axisTitleSize), colour=axisTitleColor),
            legend.text = element_text(size=rel(legendLabelSize), colour=legendLabelColor),
            plot.title = element_text(size=rel(titleSize), colour=titleColor))
    # ------------------------------------------------------------------------------------
    # check whether only diff-line is shown or upper and lower boundaries. in the latter
    # case, show legend, else hide legend
    # ------------------------------------------------------------------------------------
    if (diff) {
      baseplot <- baseplot +
        guides(fill=FALSE)
    }else{
      baseplot <- baseplot +
        scale_colour_manual(values=c(lowerBoundColor, upperBoundColor), name="", labels=lLabels)
    }
    # ------------------------------------------------------------------------------------
    # apply specific border/theme properties
    # ------------------------------------------------------------------------------------
    # the panel-border-property can only be applied to the bw-theme
    if (!is.null(borderColor)) {
      if (!is.null(theme) && theme=="bw") {
        baseplot <- baseplot + 
          theme(panel.border = element_rect(colour=borderColor))
      }else{
        print("Parameter 'borderColor' can only be applied to 'bw' theme.")
      }
    }
    if (!is.null(axisColor)) {
      baseplot <- baseplot + 
        theme(axis.line = element_line(colour=axisColor))
    }
    if (!is.null(minorgrid)) {
      baseplot <- baseplot + 
        theme(panel.grid.minor = minorgrid)
    }
    if (!is.null(majorgrid)) {
      baseplot <- baseplot + 
        theme(panel.grid.major = majorgrid)
    }
    if (hideGrid.x) {
      baseplot <- baseplot + 
        theme(panel.grid.major.x = hidegrid,
              panel.grid.minor.x = hidegrid)
    }
    if (hideGrid.y) {
      baseplot <- baseplot + 
        theme(panel.grid.major.y = hidegrid,
              panel.grid.minor.y = hidegrid)
    }
    # ------------------------------------------------------------------------------------
    # plot final object
    # ------------------------------------------------------------------------------------
    print(baseplot)
  }
  return('interaccion')
}
# -------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
#
# -------------------------------------------------------------------------------
sem.fnc=function(datos=NULL, modelo=NA, estimador='ML', 
    titulo='Standard solution',lmtest=TRUE, moment=FALSE, 
    residual=FALSE,  grupo = NULL, tipo='sem', et='default', 
    grupos.iguales= "", ortogonal = FALSE, grafica=FALSE, 
    to.pdf=FALSE, covarianza=NULL, medias=NULL, n=NULL){
  require(lavaan,  quietly =TRUE)
 
  if(is.na(modelo[1])){
    cat('*** Error. Es obligatorio especificar un modelo a estimar ***',fill=TRUE)
    cat(' ',fill=TRUE)
    cat("modelo= ' # latent variable definitions",fill=TRUE)
    cat(' ind60 =~ x1 + x2 + x3',fill=TRUE)
    cat(' dem60 =~ y1 + a*y2 + b*y3 + c*y4',fill=TRUE)
    cat(' dem65 =~ y5 + a*y6 + b*y7 + c*y8',fill=TRUE)
    cat(' ',fill=TRUE)
    cat('# regressions',fill=TRUE)
    cat(' dem60 ~ ind60',fill=TRUE)
    cat(' dem65 ~ ind60 + dem60',fill=TRUE)
    cat(' ',fill=TRUE)
    cat('# residual correlations',fill=TRUE)
    cat(' y1 ~~ y5',fill=TRUE)
    cat(' y2 ~~ y4 + y6',fill=TRUE)
    cat(' y3 ~~ y7',fill=TRUE)
    cat(' y4 ~~ y8',fill=TRUE)
    cat(" y6 ~~ y8'",fill=TRUE)
    cat('',fill=TRUE)
    cat(' sem.fnc(PoliticalDemocracy, modelo=modelo, grafica=T)',fill=TRUE)
    cat(' ',fill=TRUE)
    stop()
  }
  
  if(!is.null(covarianza[1]) & is.null(n)){
    cat(' ',fill=TRUE)
    cat('*** Error. Has incluido una matriz de varianzas y covarianzas y no has indicado sample size. ',fill=TRUE)
    cat("*** Incluyelo en el argumento n. Ej: covarianza='mi.matriz', n=300",fill=TRUE)
    cat(' ',fill=TRUE)
    stop()
  }    
  
  # AJUSTES
  if(tipo=='sem'){
    fit.mod1= sem(modelo, meanstructure = moment,group=grupo, 
      data=datos,estimator=estimador, se=et,
      group.equal =grupos.iguales, orthogonal=ortogonal,
      sample.cov=covarianza, sample.nobs=n, sample.mean=medias)
  }  
  if(tipo=='afc'){
    fit.mod1= cfa(modelo, meanstructure = moment, group=grupo, 
      data=datos,estimator=estimador, se=et,
      group.equal =grupos.iguales, orthogonal=ortogonal,
      sample.cov=covarianza, sample.nobs=n, sample.mean=medias)
  }
  if(tipo=='crecimiento'){
    fit.mod1= growth(modelo, group=grupo, 
      data=datos,estimator=estimador, se=et,
      group.equal =grupos.iguales, orthogonal=ortogonal,
      sample.cov=covarianza, sample.nobs=n, sample.mean=medias)
  }  
  
  if(grafica){
	require(qgraph,  quietly =TRUE)
  	if(to.pdf){
  		crea.pdf.fnc('graf_sem.pdf',apaisado=T)
  			qgraph(fit.mod1, vsize.man=5,titles=F); 
  			title(titulo, line=2.5)
  		cierra.pdf.fnc()
  	}else{
  	  	x11(); print(qgraph(fit.mod1,vsize.man=5,titles=F))
 			title(titulo, line=2.5)	  	
  	}
  }
  ajustes= data.frame(ajuste= fitMeasures(fit.mod1,
	c('chisq','df','pvalue','nfi','nnfi','cfi','gfi','agfi',
	'rmsea','rmsea.ci.lower','rmsea.ci.upper')))
  ajustes=t(round(ajustes,3))
  
  if(lmtest){
    mi=modindices(fit.mod1)
    cargas=mi[mi$op=='=~',]
    errores=mi[mi$op=='~~',] 
    cargas=  ordena.por.variable.fnc(cargas, 'mi', descendente=T, silente=TRUE)
    errores= ordena.por.variable.fnc(errores, 'mi', descendente=T, silente=TRUE) 
    que.col=match('indice',names(cargas))
    cargas=cargas[,-que.col]
    errores=errores[,-que.col] 
  }
  crea.cat.fnc('MODELO ESTIMADO')
  print(summary(fit.mod1,  rsquare=T, fit.measures=T))
  crea.cat.fnc('AJUSTE')
  print(ajustes)
  crea.cat.fnc('INDICES DE MODIFICACION CARGAS')
  print(head(cargas,20))
  crea.cat.fnc('INDICES DE MODIFICACION ERRORES')
  print(head(errores,20))
  crea.cat.fnc('SOLUCION ESTANDARIZADA')
  print(standardizedSolution(fit.mod1))	
  stand=standardizedSolution(fit.mod1)
  nombres=stand[stand$op=='=~','rhs']
  index=names(datos) %in% nombres
  
  residuales=resid(fit.mod1, type='cor')
  if(residual){ 
    crea.cat.fnc('Residuales'); 
    print(residuales)
    res=residuales$cor
    res2=stack(data.frame(unclass(res)))
    res2$que.var=rep(row.names(res),dim(res)[2])
    res2=elimina.repetidos.fnc(res2, ID='values')
    res2=ordena.por.variable.fnc(res2,1, silente=TRUE, descendente=TRUE)
    res2$maximo=paste('corr(',res2$que.var,'-',res2$ind,')',sep='')
    row.names(res2)=NULL
    print(res2[1:5,c('maximo','values')])
  }
  
  mardia_=try(mardia(datos[,names(datos)[index]],plot=FALSE),silent=TRUE)
		que.clase=class(mardia_)
		if(que.clase[1] !='try-error'){
     			mardia_=data.frame(matrix(c(mardia_$b1p, mardia_$skew, mardia_$p.skew,
		 		mardia_$b2p, mardia_$kurtosis, mardia_$p.kurt),
				byrow=TRUE, nrow=2))
			row.names(mardia_)=c('Asimetria','Apuntamiento')
			names(mardia_)=c('Mardia','Valor','p.val')
		}else{
			mardia_='No se han podido calcular los estadisticos de mardia'
		}
  crea.cat.fnc('NORMALIDAD MULTIVARIADA')		
  print(mardia_)
  cat('',fill=TRUE)
  return(fit.mod1)
  try(detach(package:lavaan),silent=TRUE)
  try(detach(package:qgraph),silent=TRUE)
}
# -------------------------------------------------------------------------------

#---------------------------------------------------------------------------
# Extraida de fa.parallel from psych library
#---------------------------------------------------------------------------
 fa.parallel_=function (x, n.obs = NULL, fm = "minres", fa = "both", main = "Parallel Analysis Scree Plots", 
    n.iter = 20, error.bars = FALSE, SMC = FALSE, ylabel = NULL, 
    show.legend = TRUE) {
    cl <- match.call()
    if (!require(parallel)) {
        message("The parallel package needs to be installed to run mclapply")
    }
    ci <- 1.96
    arrow.len <- 0.05
    nsub <- dim(x)[1]
    nvariables <- dim(x)[2]
    if (!is.null(n.obs)) {
        nsub <- n.obs
        rx <- x
        if (dim(x)[1] != dim(x)[2]) {
            warning("You specified the number of subjects, implying a correlation matrix, but do not have a correlation matrix, correlations found ")
            rx <- cor(x, use = "pairwise")
        }
    }
    else {
        if (nsub == nvariables) {
            warning("It seems as if you are using a correlation matrix, but have not specified the number of cases. The number of subjects is arbitrarily set to be 100  ")
            rx <- x
            nsub = 100
            n.obs = 100
        }
        else {
            rx <- cor(x, use = "pairwise")
        }
    }
    valuesx <- eigen(rx)$values
    if (SMC) {
        diag(rx) <- smc(rx)
        fa.valuesx <- eigen(rx)$values
    }
    else {
        fa.valuesx <- fa(rx, fm = fm, warnings = FALSE)$values
    }
    temp <- list(samp = vector("list", n.iter), samp.fa = vector("list", 
        n.iter), sim = vector("list", n.iter), sim.fa = vector("list", 
        n.iter))
    templist <- mclapply(1:n.iter, function(XX) {
        if (is.null(n.obs)) {
            sampledata <- matrix(apply(x, 2, function(y) sample(y, 
                nsub, replace = TRUE)), ncol = nvariables)
            values.samp <- eigen(cor(sampledata, use = "pairwise"))$values
            temp[["samp"]] <- values.samp
            if (fa != "pc") {
                if (SMC) {
                  sampler <- cor(sampledata, use = "pairwise")
                  diag(sampler) <- smc(sampler)
                  temp[["samp.fa"]] <- eigen(sampler)$values
                }
                else {
                  temp[["samp.fa"]] <- fa(cor(sampledata, use = "pairwise"), 
                    fm = fm, SMC = FALSE, warnings = FALSE)$values
                }
            }
        }
        simdata = matrix(rnorm(nsub * nvariables), nrow = nsub, 
            ncol = nvariables)
        sim.cor <- cor(simdata)
        temp[["sim"]] <- eigen(sim.cor)$values
        if (fa != "pc") {
            if (SMC) {
                diag(sim.cor) <- smc(sim.cor)
                temp[["sim.fa"]] <- eigen(sim.cor)$values
            }
            else {
                fa.values.sim <- fa(sim.cor, fm = fm, SMC = FALSE, 
                  warnings = FALSE)$values
                temp[["sim.fa"]] <- fa.values.sim
            }
        }
        replicates <- list(samp = temp[["samp"]], samp.fa = temp[["samp.fa"]], 
            sim = temp[["sim"]], sim.fa = temp[["sim.fa"]])
    })
    if (is.null(ylabel)) {
        if (fa != "pc") {
            ylabel <- "eigenvalues of principal components and factor analysis"
        }
        else {
            ylabel <- "eigen values of principal components"
        }
    }
    values <- t(matrix(unlist(templist), ncol = n.iter))
    values.sim.mean = colMeans(values)
    values.sim.se <- apply(values, 2, sd)/sqrt(n.iter)
    ymax <- max(valuesx, values.sim.mean)
    sim.pc <- values.sim.mean[1:nvariables]
    sim.fa <- values.sim.mean[(nvariables + 1):(2 * nvariables)]
    sim.pcr <- values.sim.mean[(2 * nvariables + 1):(3 * nvariables)]
    sim.far <- values.sim.mean[(3 * nvariables + 1):(4 * nvariables)]


    if (fa == "pc") {
        results <- list(fa.values = fa.valuesx, pc.values = valuesx, 
            pc.sim = sim.pc, Call = cl)
        fa.test <- NA
    }
    else {
        results <- list(fa.values = fa.valuesx, fa.sim = sim.fa, 
            pc.values = valuesx, pc.sim = sim.pc, Call = cl)
        fa.test <- which(!(fa.valuesx > sim.fa))[1] - 1
        results$nfact <- fa.test
    }
    pc.test <- which(!(valuesx > sim.pc))[1] - 1
    results$ncomp <- pc.test

	return(fa.test)
}
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#
#---------------------------------------------------------------------------
fiabilidad.interjueces.fnc=function(datos, item.x.jueces=TRUE,
												modelo='oneway'){
	require(irr, quietly=TRUE)
	crea.cat.fnc('FIABILIDAD INTERJUECES')
	modelo='oneway'
	if(item.x.jueces){
		print(icc(datos, model=modelo, type='agreement'))
	}else{
		datos=data.frame(t(datos))
		print(icc(datos, model=modelo, type='agreement'))
	}
	try(detach(package:irr),silent=TRUE)
}
#fiabilidad.interjueces.fnc(anxiety)
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
# Check si existe el caracter - en los nombres de los niveles de los factores
#---------------------------------------------------------------------------
 check.niveles.fnc=function(datos, fac.inter=NA, fac.intra=NA){
	if(!is.na(fac.inter[1])){
		for(i in 1:length(fac.inter)){
			niveles=with(datos, as.character(unique(datos[,fac.inter[i]])))
			chivato=sum(do.call(rbind,lapply(strsplit(as.character(niveles),'-'), length)))
			if(chivato > length(as.character(unique(datos[,fac.inter[i]])))){
				cat('', fill=TRUE)
				cat('*** Error. Los niveles de los factores no deben contener el caracter -    ***', fill=TRUE)
				cat('*** Recodifica dicho factor eliminando ese caracter del nombre del nivel. ***', fill=TRUE)
				cat('*** Niveles del factor',fac.inter[i],':',niveles,fill=TRUE)
				cat('*** Consulta como recodificar una variable en:                            ***', fill=TRUE)
				cat('*** https://sites.google.com/site/ullrtoolbox/02-manipulacion-de-datos/recodificar',fill=TRUE)
				cat('', fill=TRUE)
				stop( )
			}
		}
	}
	if(!is.na(fac.intra[1])){
		for(i in 1:length(fac.intra)){
			niveles= fac.intra[[i]]
			chivato=sum(do.call(rbind,lapply(strsplit(as.character(niveles),'-'), length)))
			if(chivato > length(fac.intra[[i]])  ){
				cat('', fill=TRUE)
				cat('*** Error. Los niveles de los factores no deben contener el caracter -    ***', fill=TRUE)
				cat('*** Recodifica dicho factor eliminando ese caracter del nombre del nivel. ***', fill=TRUE)
				cat('*** Niveles del factor',names(fac.intra)[i],':',niveles,fill=TRUE)
				cat('*** Consulta como recodificar una variable en:                            ***', fill=TRUE)
				cat('*** https://sites.google.com/site/ullrtoolbox/02-manipulacion-de-datos/recodificar',fill=TRUE)
				cat('', fill=TRUE)
				stop( )
			}
		}
	}
 }
#---------------------------------------------------------------------------
    


