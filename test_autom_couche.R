# définition des variables pour les calculs par profil (mod,min, max)
var_pstat <- c("rp_95_nom_strate")
d_profil2 <- donnee
i<-levels(as.factor(d_profil2[,c("id_utt")]))
#séparation du tableau de données par UTT
dpm<- split(d_profil2,d_profil2[,c("id_utt")]);
#calcul des statistiques par UTT
resnom <- matrix(NA,nrow=length(dpm),ncol=length(unique(donnee$rp_95_nom_strate)))
colnames(resnom) <- unique(donnee$rp_95_nom_strate)
nomst <- lapply(1:length(dpm), FUN =function(m){
      #crétation du vecteur z pour chaque variable quali
      # pour chaque valeur prise par la variable n on fait la somme des surfaces
      z <- paste(var=as.character(unique(dpm[[m]][,which(colnames(dpm[[m]])==var_pstat)])),
                            surf=as.numeric(lapply(1:length(unique(dpm[[m]][,which(colnames(dpm[[m]])==var_pstat)])),function(p){
                              sum=sum(
                                dpm[[m]][which(dpm[[m]][,which(colnames(dpm[[m]])==var_pstat)]==
                                                  unique(dpm[[m]][,which(colnames(dpm[[m]])==var_pstat)])[p]),]$surf)
                            })),collapse = " / ")
})
resnom=cbind.data.frame(id_utt=c(i),no_strate_mod= matrix(unlist(nomst),nrow=length(dpm),byrow=TRUE))
      #mod du max no_strate par utt
nst <- lapply(1:length(dpm), FUN =function(m){
   z <- matrix(
    unlist(lapply(1:length(unique(dpm[[m]]$id_utsutt)), FUN =function(n){
      #crétation du vecteur z pour chaque variable quali
      # pour chaque valeur prise par la variable n on fait la somme des surfaces
      cbind.data.frame(var=max(dpm[[m]][which(dpm[[m]]$id_utsutt==unique(dpm[[m]]$id_utsutt)[[n]]),]$no_strate),
                            surf=unique(dpm[[m]][which(dpm[[m]]$id_utsutt==unique(dpm[[m]]$id_utsutt)[[n]]),]$surf))
 #(le collapse permet de coller deux valeurs ayant la même surface max)
    })),ncol=2,byrow=T)
  paste(z[z[,2]==max(z[,2]),1],collapse="|")
})

resstrate=cbind.data.frame(id_utt=c(i),no_strate_mod= matrix(unlist(nst),ncol=1,byrow=TRUE))

rescouche <- merge(resstrate,resnom,by="id_utt")
write.csv2(rescouche,file="couche.csv",row.names = FALSE)
