############################################################
# Script d'automatisation du calcul des variables TypTerre #
# Chloé SWIDERSKI - Avril 2016                             #
############################################################

#NB: drainage calcule en modale et en dominante

# ##prérequis :
# fournir le datamart de l'étude (brute, extrait de Donesol)
# fournir la table de jointure UTT/couches & UTS/strates : il doit contenir
# -la colone id_utt (nom des unités TypTerre)
# -la colonne h (numéro de la couche du profil Typterre)

#documents par départements
# chemin <- "D:/Donnees/autom/crenouard"; datamart <- "vienne_datamart.csv";doc_joint <- "vienne_joint.csv"
# chemin <- "D:/Donnees/autom/crenouard"; datamart <- "2sevres_datamart.csv";doc_joint <- "2sevres_joint.csv"
# chemin <- "D:/Donnees/autom/jmoulin"; datamart <- "indre_datamart.csv";doc_joint <- "indre_joint.csv"
# chemin <-  "D:/Donnees/autom/cducommun"; datamart <- "vendee_datamart.csv";doc_joint <- "vendee_joint.csv"
# chemin <- "D:/Donnees/autom/jpparty";datamart <- "alsace_datamart.csv";doc_joint <- "alsace_joint.csv"

#concatenation des differents tableaux des départements entre-ouest
setwd("D:/Donnees/autom/centre_ouest")
write.csv2(rbind(read.csv2(file="vienne_datamart.csv",head=T),read.csv2(file="indre_datamart.csv",head=T),
                 read.csv2(file="2sevres_datamart.csv",head=T),read.csv2(file="vendee_datamart.csv",head=T)),file="centre_ouest_datamart.csv",row.names = FALSE)
write.csv2(rbind(read.csv2(file="vienne_joint.csv",head=T)[,c("id_uts","no_uts","no_strate","h","id_utt")],
                 read.csv2(file="indre_joint.csv",head=T)[,c("id_uts","no_uts","no_strate","h","id_utt")],
                 read.csv2(file="2sevres_joint.csv",head=T)[,c("id_uts","no_uts","no_strate","h","id_utt")],
                 read.csv2(file="vendee_joint.csv",head=T)[,c("id_uts","no_uts","no_strate","h","id_utt")]),file="centre_ouest_joint.csv",row.names = FALSE)
datamart <- "centre_ouest_datamart.csv"
doc_joint <-"centre_ouest_joint.csv"

# La fonction statistique appel 3 variables:
# - chemin : l'environnement de travail
# - datamart : le nom du fichier csv datamart
# - docjoint : le nom du fichier CSV qui fait la jointure UTT/couche & UTS/strates

# mettre le script fusion_meth.R dans le dossier de travail

statistique_utt <- function(chemin,datamart,docjoint){
#################################

setwd(chemin)  #chargement de l'environnement de travail
source("gestion_meth.r") #fonction de concaténation des méthodes
  ###UILITAIRES###
install.packages("data.table", dependencies = FALSE);install.packages("modeest", dependencies = FALSE)
install.packages("plyr", dependencies = FALSE);library(data.table);
library(modeest);library(plyr)

### fonction pour calcul du mode###
mod <- function(data,variab){
  z <-cbind.data.frame(var=as.character(unique(data[,which(colnames(data)==variab)])),
                       surf=as.numeric(lapply(1:length(unique(data[,which(colnames(data)==variab)])),function(p){
                         sum=sum(
                           data[which(data[,which(colnames(data)==variab)]==
                                        unique(data[,which(colnames(data)==variab)])[p]),]$surf)
                       })))
  #On ne conserve que la valeur qui comptabilise la surface max 
  return(paste(z[z[,2]==max(z[,2]),1],collapse="|"))}

# 1- Import des données
#######################
t_var <- read.csv2(file=datamart,head=T)
t_utt <- read.csv2(file=doc_joint,head=T)
#on enlève les strate non affectées à un horizon typterres
t_utt <- t_utt[!is.na(t_utt$h),]

# 2- Nettoyage du datamart et concaténation des méthodes
########################################################
toto <- net_data(t_var) #concaténation des méthodes avec la fonction concat_meth
t_vart <- toto[[1]]
var_quant <- toto[[2]]
rm(toto)

# 3- Jointure des tables
########################
donnee <- merge(t_vart,t_utt[,c("id_uts","no_uts","no_strate","h","id_utt")],by.x=c("id_uts","no_strate"),by.y=c("id_uts","no_strate"), all.x=T) #all.y induit des NA
donnee <- donnee[!is.na(donnee$id_utt),] # pour "réparer le all.x
#verif
if(nrow(donnee)>1){print("jointure ok")}else{print("probleme de jointure")}

#creation des identifiants unique UTT-H / UTS-UTT
donnee$id_utth <- (paste(donnee$id_utt,donnee$h,sep=""))
donnee$id_utsutt <- (paste(donnee$id_uts,donnee$id_utt,sep=""))


# 4- Calcul des surfaces
########################
#surface unitaire de l'UCS x pourcentage de l'uts dans l'UCS (en valeur entière donc diviser par cent)
donnee$surf <- round((donnee$surf_unit * donnee$pourcent)/100,1)
#verif
if(nrow(donnee[is.na(donnee$surf),])==0){print("surface ok")}else{print("probleme de jointure : surface d'utt nulle")}


# 5- statistiques des variables quantitatives
##############################################
 # définition des variables pour les calculs par couple utt_h
i<-levels(as.factor(donnee[,c("id_utth")]))
#séparation du tableau de données par UTT/H
ddq<- split(donnee,donnee[,c("id_utth")]);
#calcul des statistiques par UTT/H
qstat <- lapply(1:length(ddq), FUN =function(m){
  matrix(
    unlist(lapply(1:length(var_quant), FUN =function(n){
    t(c(
      # calcul du mod
      moyP = round(sum(unlist(lapply(1:nrow(ddq[[m]]),FUN=function(p){
        # on somme pour chaque UTS (dpm[[m]])qui composent l'UTT on fait :surface * valeur de le variable n 
        ddq[[m]][p,]$surf * ddq[[m]][p,which(colnames(ddq[[m]])==var_quant[n])]
      })),na.rm=T)/sum( ddq[[m]][!is.na(ddq[[m]][,which(colnames(ddq[[m]])==var_quant[n])]),]$surf),1),
      # et on divise par  somme des surfaces de toutes les UTS pour lesquelles variable n n'est pas nulle
      ### mod : 
        #crétation du vecteur z pour chaque variable quali
            # pour chaque valeur prise par la variable n on fait la somme des surfaces
      modal = mod(data=ddq[[m]],variab = var_quant[n]), #(le collapse permet de coller deux valeurs ayant la même surface max)
         
      #calcul du min,max, variance et écart-type
      min = round(min(ddq[[m]][,which(colnames(ddq[[m]])==var_quant[n])],na.rm=T),1),
      max = round(max(ddq[[m]][,which(colnames(ddq[[m]])==var_quant[n])],na.rm=T),1),
      var = round(var(ddq[[m]][,which(colnames(ddq[[m]])==var_quant[n])],na.rm=T),1),
      sd = round(sd(ddq[[m]][,which(colnames(ddq[[m]])==var_quant[n])],na.rm=T),1)))
  })),nrow=6,byrow=F)
})
# mise en forme du tableau de donnees : id_utth + id_utt + h + résultats de la liste qstat
resq=cbind.data.frame(c(i),substr(i,1,nchar(i)-1),substr(i,nchar(i),nchar(i)),
                      matrix(unlist(qstat),ncol=length(var_quant)*6,byrow=TRUE))
#remplacement des infinis par des NA
is.na(resq) <- sapply(resq, is.infinite)
# nommer les colonnes
colnames(resq) <- c("id_utth","id_utt","h",paste(rep(var_quant,each=6),rep(c("moyP","mode","min","max","var","ec-typ"),length(var_quant)),sep="-"))
#réinitialisation des variables intermédiaires
rm(ddq);rm(i);rm(qstat)
print("stats quanti utt/couche effectuees")

# définition des variables pour les calculs par profil (somme des observations/profils faits par UTT)
var_psum <- c("nb_obs","nb_prof_uni")
d_profil <- donnee[!duplicated(donnee$id_utsutt),]
i<-levels(as.factor(d_profil[,c("id_utt")]))
#séparation du tableau de données par UTT
dpr<- split(d_profil,d_profil[,c("id_utt")]);
#calcul des sommes par UTT
psum <- lapply(1:length(dpr), FUN =function(m){
  matrix(
    unlist(lapply(1:length(var_psum), FUN =function(n){
      sum = round(sum(dpr[[m]][,which(colnames(dpr[[m]])==var_psum[n])],na.rm=T),1)
        })),nrow=1,byrow=T)
})
# définition des variables pour les calculs par profil (mod,min, max)
var_pstat <- c("prof_sol_min","prof_sol_mod","prof_sol_max", "appar_mat1_mod","pierro_surf","niveau_nap_mod","pente_moy")
#calcul des statistiques par UTT
pstat <- lapply(1:length(dpr), FUN =function(m){
  matrix(
    unlist(lapply(1:length(var_pstat), FUN =function(n){
      t(c(
        # calcul du mod
      moyP = round(sum(unlist(lapply(1:nrow(dpr[[m]]),FUN=function(p){
        # on somme pour chaque UTS (dpm[[m]])qui composent l'UTT on fait :surface * valeur de le variable n 
        dpr[[m]][p,]$surf * dpr[[m]][p,which(colnames(dpr[[m]])==var_pstat[n])]
      })),na.rm=T) /sum(dpr[[m]][!is.na(dpr[[m]][,colnames(dpr[[m]])==var_pstat[n]]),]$surf),1),
      # et on divise par  somme des surfaces de toutes les UTS pour lesquelles variable n n'est pas nulle
      modal = mod(data=dpr[[m]],variab = var_pstat[[n]]),
      #calcul du min,max, variance et écart-type
      min = round(min(dpr[[m]][,which(colnames(dpr[[m]])==var_pstat[n])],na.rm=T),1),
      max = round(max(dpr[[m]][,which(colnames(dpr[[m]])==var_pstat[n])],na.rm=T),1),
      var = round(var(dpr[[m]][,which(colnames(dpr[[m]])==var_pstat[n])],na.rm=T),1),
      sd = round(sd(dpr[[m]][,which(colnames(dpr[[m]])==var_pstat[n])],na.rm=T),1)))
    })),nrow=6,byrow=F)
})
# mise en forme du tableau de donnees : id_utt + résultats de la liste psum + pstat
resp=cbind.data.frame(c(i),matrix(unlist(psum),ncol=length(var_psum),byrow=TRUE),
                      matrix(unlist(pstat),ncol=length(var_pstat)*6,byrow=TRUE))
#remplacement des infini par des NA
is.na(resp) <- sapply(resp, is.infinite)
# nommer les colonnes
colnames(resp) <- c("id_utt",c(var_psum),paste(rep(var_pstat,each=6),rep(c("moyP","mode","min","max","var","ec_typ"),length(var_pstat)),sep="-"))
#réinitialisation des variables intermédiaires
rm(dps);rm(psum);rm(i);rm(pstat);rm(dpr)
print("stats quanti utt effectuees")

# 6- Dominantes des  variables qualitatives
###########################################
#définition des variables qualitatives à prendre en compte
var_qual <- c("type_relief","domai_morpho1","nom_local_uts","cpcs_nom_uts","fao_2007_nom" ,"rp_95_nom_uts","rp_2008_nom_uts","org_geol","classe_mat1","nom_eg1","nom_eg2" ,"pierro_surf","taille_eg1","taille_eg2","pierro_eg1","pierro_eg2","salure_uts","reg_hydri" ,"exces_eau1","orig_exces","drai_nat","nat_dis","forme_dis","conseq_dis" ,"cap_retention","compacite","contrainte" ,"couleur","effervescence","nom_eg","porosite_strate","texture_aisne" ,"texture_geppa","type_structure")
i<-levels(as.factor(donnee[,c("id_utth")]))
# séparation du tableau de données par UTT/H
ddqa<- split(donnee,donnee[,c("id_utth")]);
#calcul des dominntes par UTT/H
qdom <- lapply(1:length(ddqa), FUN =function(m){
  matrix(
    unlist(lapply(1:length(var_qual), FUN =function(n){
      #crétation du vecteur z pour chaque variable quali
        # pour chaque valeur prise par la variable n on fait la somme des surfaces
      modal = mod(data = ddqa[[m]],variab = var_qual[[n]]) #(le collapse permet de coller deux valeurs ayant la même surface max)
      })),nrow=1,byrow=F)
})
# mise en forme du tableau de donnees : id_utth + id_utt + h + résultats de la liste qdom
resqual=cbind.data.frame(c(i),substr(i,1,nchar(i)-1),substr(i,nchar(i),nchar(i)),
                         matrix(unlist(qdom),ncol=length(var_qual),byrow=TRUE))
# nommer les colonnes
colnames(resqual) <- c("id_utth","id_utt","h",c(var_qual))
#réinitialisation des variables intermédiaires
rm(ddqa);rm(qdom);rm(i)
print("dominantes effectuees")

# 7- Sauvegarde des surfaces des uts par UTT
##########
ressurf <- ddply(aggregate(surf~id_utt+id_uts,sum,data=unique(donnee[,c("surf","id_utt","id_uts")])),
                 .(id_utt), transform, pourc_surf = round((surf/sum(surf))*100,1),sum_surf=sum(surf))

#restitution des tableaux de résultats calcules dans la fonction
return(donnee,resp,resq,resqual,ressurf)
}


# 8 - Sauvegarde environnement R + tableaux en csv
# save.image("stat_utt_indre.RData")
# save.image("stat_utt_vendee.RData")
# save.image("stat_utt_vienne.RData")
# save.image("stat_utt_2sevres.RData")
# save.image("stat_utt_alsace.RData")
save.image("stat_utt_centreouest.RData")
write.csv2(resp,file="stat_prof.csv",row.names = FALSE) 
write.csv2(resq,file="stat_h.csv",row.names = FALSE) 
write.csv2(resqual,file="dominante_h.csv",row.names = FALSE)
write.csv2(ressurf,file="surf.csv",row.names = FALSE)

