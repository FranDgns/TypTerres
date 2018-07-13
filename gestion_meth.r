net_data <- function(d_var){

#----------------------------------#
#selection des colonnes necessaires#
#----------------------------------#
  d_var <- d_var[,c("id_etude","no_etude","id_ucs","no_ucs","id_uts","no_uts","no_strate","surf_unit","type_relief",
                    "domai_morpho1","nom_local_uts","nb_obs","cpcs_nom_uts","fao_2007_nom","rp_95_nom_uts",
                    "rp_2008_nom_uts","nb_prof_uni","org_geol","prof_sol_min","prof_sol_mod","prof_sol_max",
                    "classe_mat1","appar_mat1_mod","epais_moy","pierro_surf","nom_eg1","nom_eg2","taille_eg1","taille_eg2",
                    "pierro_eg1","pierro_eg2","salure_uts","reg_hydri","exces_eau1","orig_exces","niveau_nap_mod",
                    "appar_g_mod","appar_g_max","appar_go_min","appar_go_mod","appar_gr_mod","appar_h_mod","drai_nat"
                    ,"nat_dis","forme_dis","conseq_dis","prof_dis_mod","cap_retention","prof_rac_mod","pourcent",
                    "pente_moy","prof_appar_min","prof_appar_moy","prof_appar_max","abondance_eg_0",
                    "abondance_eg_sansmeth","abondance_eg_p_0","abondance_eg_prin_0","abondance_eg_prin_sansmeth",
                    "abondance_eg_sec_0","abondance_eg_sec_sansmeth","al_ech_0","al_ech_37","al_ech_40_2",
                    "al_ech_40_3","al_ech_41","al_ech_41_1","al_ech_sansmeth","ca_ech_0","ca_ech_0_1","ca_ech_40",
                    "ca_ech_40_2","ca_ech_40_3","ca_ech_40_5","ca_ech_41","ca_ech_41_1","ca_ech_41_2","ca_ech_47_1_1",
                    "ca_ech_sansmeth","calc_act_0","calc_act_4","calc_act_4_1","calc_act_4_2","calc_act_sansmeth",
                    "calc_tot_0","calc_tot_1","calc_tot_1_1","calc_tot_2","calc_tot_2_1","calc_tot_2_1_1",
                    "calc_tot_2_1_2","calc_tot_3","calc_tot_sansmeth","carbone_0","carbone_120","carbone_16",
                    "carbone_16_1","carbone_16_2","carbone_16_2_2","carbone_16_5","carbone_16_5_1","carbone_16_5_2",
                    "carbone_17_1","carbone_17_2","carbone_17_8","carbone_19","carbone_19_1","carbone_19_4",
                    "carbone_sansmeth","ca_tot_0","ca_tot_53_1_4","ca_tot_56","ca_tot_sansmeth","cd_tot_0",
                    "cd_tot_0_1","cd_tot_53","cd_tot_53_1","cd_tot_53_6","cd_tot_sansmeth","cec_0","cec_0_1",
                    "cec_113","cec_126","cec_37","cec_38","cec_39","cec_40","cec_40_1","cec_40_2","cec_40_5",
                    "cec_41","cec_41_5_1","cec_41_6","cec_41_7","cec_41_8","cec_90","cec_sansmeth","c_n_0",
                    "cond_hydro_lab_0","cond_hydro_lab_sansmeth","cond_hydro_ter_sansmeth","dens_appar_0",
                    "dens_appar_0_1","dens_appar_133","dens_appar_172","dens_appar_sansmeth","fe_ech_0",
                    "fe_ech_40_2","fe_ech_40_3","fe_ech_40_5","fe_ech_41","fe_ech_47_1_1","fe_ech_sansmeth",
                    "fe_lib_0","fe_lib_49","fe_lib_49_1","fe_lib_49_2","fe_lib_50","fe_lib_51","fe_lib_72",
                    "fe_lib_72_1","fe_lib_75","fe_lib_sansmeth","fe_tot_0","fe_tot_0_1","fe_tot_53","fe_tot_53_1",
                    "fe_tot_53_1_4","fe_tot_53_2_1","fe_tot_53_3","fe_tot_56_1","fe_tot_63","fe_tot_sansmeth",
                    "hg_tot_0","hg_tot_0_1","hg_tot_53","hg_tot_90","hg_tot_sansmeth","humid_pf2_0",
                    "humid_pf2_sansmeth","humid_pf25_0","humid_pf25_151","humid_pf25_sansmeth","humid_pf3_0",
                    "humid_pf3_215","humid_pf3_sansmeth","humid_pf42_0","humid_pf42_151","humid_pf42_sansmeth",
                    "humid_pf43_0","humid_pf43_sansmeth","indice_stab_0","indice_stab_sansmeth","k1_0",
                    "k1_230","k1_sansmeth","k2_0","k2_230","k2_sansmeth","k3_0","k3_230","k_ech_0","k_ech_0_1",
                    "k_ech_36","k_ech_40","k_ech_40_2","k_ech_40_3","k_ech_41","k_ech_41_1","k_ech_41_2","k_ech_47",
                    "k_ech_sansmeth","k_tot_0","k_tot_0_1","k_tot_53_1_4","k_tot_sansmeth","la_sansmeth",
                    "lim_fin_tot_0","lim_fin_tot_sansmeth","li_tot_sansmeth","lr_sansmeth","mg_ech_0","mg_ech_0_1",
                    "mg_ech_40","mg_ech_40_2","mg_ech_40_3","mg_ech_40_5","mg_ech_41","mg_ech_41_1",
                    "mg_ech_41_2","mg_ech_47","mg_ech_47_1_1","mg_ech_sansmeth","mg_tot_0","mg_tot_0_1",
                    "mg_tot_53_1_4","mg_tot_sansmeth","mn_ech_0","mn_ech_40","mn_ech_40_2","mn_ech_40_3",
                    "mn_ech_41","mn_ech_41_3","mn_ech_sansmeth","mn_lib_0","mn_lib_72","mn_lib_sansmeth",
                    "mn_tot_0","mn_tot_53","mn_tot_53_1","mn_tot_53_1_4","mn_tot_53_3","mn_tot_56_1","mn_tot_sansmeth",
                    "mo_tot_0","mo_tot_0_1","mo_tot_53_6","mo_tot_sansmeth","na_ech_0","na_ech_0_1","na_ech_40",
                    "na_ech_40_2","na_ech_40_3","na_ech_40_4","na_ech_41","na_ech_41_1","na_ech_41_2","na_ech_47",
                    "na_ech_sansmeth","na_tot_0","na_tot_0_1","na_tot_53_1_4","na_tot_sansmeth","ni_tot_0",
                    "ni_tot_0_1","ni_tot_53","ni_tot_53_1_4","ni_tot_sansmeth","n_tot_0","n_tot_30","n_tot_30_1",
                    "n_tot_30_1_1","n_tot_30_1_2","n_tot_31","n_tot_31_1","n_tot_sansmeth","p_ass_0","p_ass_0_1",
                    "p_ass_80","p_ass_81","p_ass_81_1","p_ass_83","p_ass_86","p_ass_86_1","p_ass_86_2","p_ass_87",
                    "p_ass_87_1","p_ass_87_2","p_ass_88","pb_tot_0","pb_tot_0_1","pb_tot_53","pb_tot_53_1",
                    "pb_tot_53_6","pb_tot_56","pb_tot_sansmeth","ph_cacl2_0","ph_eau_0","ph_eau_6","ph_eau_6_1",
                    "ph_eau_7","ph_eau_7_1","ph_eau_8","ph_eau_sansmeth","p_tot_0","p_tot_53_1_4","p_tot_sansmeth",
                    "salinite_0","salinite_sansmeth","se_tot_sansmeth","si_lib_49_2","si_lib_51","si_lib_sansmeth",
                    "si_tot_0","si_tot_55","si_tot_sansmeth","sn_tot_0","sodicite_0","sodicite_sansmeth",
                    "taille_struc_p_0","taille_struc_pri_0","taille_struc_pri_sansmeth","taille_struc_s_0",
                    "taille_struc_sec_0","taille_struc_sec_sansmeth","taux_argile_0","taux_argile_sansmeth",
                    "taux_limon_0","taux_limon_sansmeth","taux_mo_0","taux_mo_17_1","taux_mo_27","taux_mo_28",
                    "taux_mo_29","taux_mo_sansmeth","taux_sable_0","taux_sable_sansmeth","ti_tot_55",
                    "ti_tot_sansmeth","tl_tot_0","tl_tot_53_6","tl_tot_sansmeth","zn_tot_0","zn_tot_0_1",
                    "zn_tot_53","zn_tot_53_1_4","zn_tot_56","zn_tot_66","zn_tot_sansmeth","compacite","contrainte",
                    "couleur","effervescence","nom_eg","porosite_strate","texture_aisne","texture_geppa",
                    "type_structure")]
 #variables quantitatives 
  v_quanti <- c("appar_g_mod","appar_g_max","appar_go_min","appar_go_mod","appar_gr_mod","appar_h_mod" ,"epais_moy","prof_dis_mod","prof_rac_mod","prof_appar_min","prof_appar_moy","prof_appar_max","abondance_eg_0","abondance_eg_sansmeth","abondance_eg_p_0","abondance_eg_prin_0","abondance_eg_prin_sansmeth",
                "drai_nat","abondance_eg_sec_0","abondance_eg_sec_sansmeth","al_ech_0","al_ech_37","al_ech_40_2",
                 "al_ech_40_3","al_ech_41","al_ech_41_1","al_ech_sansmeth","ca_ech_0","ca_ech_0_1","ca_ech_40",
                 "ca_ech_40_2","ca_ech_40_3","ca_ech_40_5","ca_ech_41","ca_ech_41_1","ca_ech_41_2","ca_ech_47_1_1",
                 "ca_ech_sansmeth","calc_act_0","calc_act_4","calc_act_4_1","calc_act_4_2","calc_act_sansmeth",
                 "calc_tot_0","calc_tot_1","calc_tot_1_1","calc_tot_2","calc_tot_2_1","calc_tot_2_1_1",
                 "calc_tot_2_1_2","calc_tot_3","calc_tot_sansmeth","carbone_0","carbone_120","carbone_16",
                 "carbone_16_1","carbone_16_2","carbone_16_2_2","carbone_16_5","carbone_16_5_1","carbone_16_5_2",
                 "carbone_17_1","carbone_17_2","carbone_17_8","carbone_19","carbone_19_1","carbone_19_4",
                 "carbone_sansmeth","ca_tot_0","ca_tot_53_1_4","ca_tot_56","ca_tot_sansmeth","cd_tot_0",
                 "cd_tot_0_1","cd_tot_53","cd_tot_53_1","cd_tot_53_6","cd_tot_sansmeth","cec_0","cec_0_1",
                 "cec_113","cec_126","cec_37","cec_38","cec_39","cec_40","cec_40_1","cec_40_2","cec_40_5",
                 "cec_41","cec_41_5_1","cec_41_6","cec_41_7","cec_41_8","cec_90","cec_sansmeth","c_n_0",
                 "cond_hydro_lab_0","cond_hydro_lab_sansmeth","cond_hydro_ter_sansmeth","dens_appar_0",
                 "dens_appar_0_1","dens_appar_133","dens_appar_172","dens_appar_sansmeth","fe_ech_0",
                 "fe_ech_40_2","fe_ech_40_3","fe_ech_40_5","fe_ech_41","fe_ech_47_1_1","fe_ech_sansmeth",
                 "fe_lib_0","fe_lib_49","fe_lib_49_1","fe_lib_49_2","fe_lib_50","fe_lib_51","fe_lib_72",
                 "fe_lib_72_1","fe_lib_75","fe_lib_sansmeth","fe_tot_0","fe_tot_0_1","fe_tot_53","fe_tot_53_1",
                 "fe_tot_53_1_4","fe_tot_53_2_1","fe_tot_53_3","fe_tot_56_1","fe_tot_63","fe_tot_sansmeth",
                 "hg_tot_0","hg_tot_0_1","hg_tot_53","hg_tot_90","hg_tot_sansmeth","humid_pf2_0",
                 "humid_pf2_sansmeth","humid_pf25_0","humid_pf25_151","humid_pf25_sansmeth","humid_pf3_0",
                 "humid_pf3_215","humid_pf3_sansmeth","humid_pf42_0","humid_pf42_151","humid_pf42_sansmeth",
                 "humid_pf43_0","humid_pf43_sansmeth","indice_stab_0","indice_stab_sansmeth","k1_0",
                 "k1_230","k1_sansmeth","k2_0","k2_230","k2_sansmeth","k3_0","k3_230","k_ech_0","k_ech_0_1",
                 "k_ech_36","k_ech_40","k_ech_40_2","k_ech_40_3","k_ech_41","k_ech_41_1","k_ech_41_2","k_ech_47",
                 "k_ech_sansmeth","k_tot_0","k_tot_0_1","k_tot_53_1_4","k_tot_sansmeth","la_sansmeth",
                 "lim_fin_tot_0","lim_fin_tot_sansmeth","li_tot_sansmeth","lr_sansmeth","mg_ech_0","mg_ech_0_1",
                 "mg_ech_40","mg_ech_40_2","mg_ech_40_3","mg_ech_40_5","mg_ech_41","mg_ech_41_1",
                 "mg_ech_41_2","mg_ech_47","mg_ech_47_1_1","mg_ech_sansmeth","mg_tot_0","mg_tot_0_1",
                 "mg_tot_53_1_4","mg_tot_sansmeth","mn_ech_0","mn_ech_40","mn_ech_40_2","mn_ech_40_3",
                 "mn_ech_41","mn_ech_41_3","mn_ech_sansmeth","mn_lib_0","mn_lib_72","mn_lib_sansmeth",
                 "mn_tot_0","mn_tot_53","mn_tot_53_1","mn_tot_53_1_4","mn_tot_53_3","mn_tot_56_1","mn_tot_sansmeth",
                 "mo_tot_0","mo_tot_0_1","mo_tot_53_6","mo_tot_sansmeth","na_ech_0","na_ech_0_1","na_ech_40",
                 "na_ech_40_2","na_ech_40_3","na_ech_40_4","na_ech_41","na_ech_41_1","na_ech_41_2","na_ech_47",
                 "na_ech_sansmeth","na_tot_0","na_tot_0_1","na_tot_53_1_4","na_tot_sansmeth","ni_tot_0",
                 "ni_tot_0_1","ni_tot_53","ni_tot_53_1_4","ni_tot_sansmeth","n_tot_0","n_tot_30","n_tot_30_1",
                 "n_tot_30_1_1","n_tot_30_1_2","n_tot_31","n_tot_31_1","n_tot_sansmeth","p_ass_0","p_ass_0_1",
                 "p_ass_80","p_ass_81","p_ass_81_1","p_ass_83","p_ass_86","p_ass_86_1","p_ass_86_2","p_ass_87",
                 "p_ass_87_1","p_ass_87_2","p_ass_88","pb_tot_0","pb_tot_0_1","pb_tot_53","pb_tot_53_1",
                 "pb_tot_53_6","pb_tot_56","pb_tot_sansmeth","ph_cacl2_0","ph_eau_0","ph_eau_6","ph_eau_6_1",
                 "ph_eau_7","ph_eau_7_1","ph_eau_8","ph_eau_sansmeth","p_tot_0","p_tot_53_1_4","p_tot_sansmeth",
                 "salinite_0","salinite_sansmeth","se_tot_sansmeth","si_lib_49_2","si_lib_51","si_lib_sansmeth",
                 "si_tot_0","si_tot_55","si_tot_sansmeth","sn_tot_0","sodicite_0","sodicite_sansmeth",
                 "taille_struc_p_0","taille_struc_pri_0","taille_struc_pri_sansmeth","taille_struc_s_0",
                 "taille_struc_sec_0","taille_struc_sec_sansmeth","taux_argile_0","taux_argile_sansmeth",
                 "taux_limon_0","taux_limon_sansmeth","taux_mo_0","taux_mo_17_1","taux_mo_27","taux_mo_28",
                 "taux_mo_29","taux_mo_sansmeth","taux_sable_0","taux_sable_sansmeth","ti_tot_55",
                 "ti_tot_sansmeth","tl_tot_0","tl_tot_53_6","tl_tot_sansmeth","zn_tot_0","zn_tot_0_1",
                 "zn_tot_53","zn_tot_53_1_4","zn_tot_56","zn_tot_66","zn_tot_sansmeth")
  
  #------------------------------------#
  # suppression des horizons organiques#
  #------------------------------------#
  
  #concatenation id_ucs et id_uts pour avoir un identifiant unique des couples UCS/UTS
  d_var$id_ucsuts <- (paste(d_var$id_ucs,d_var$no_uts,sep=","))
  # frequence des profondeur d'apparition de strate au sein du même couple UCS/UTS 
  a <- as.data.frame(table(d_var$id_ucsuts,t_var$prof_appar_moy))
  b <- a[which(a$Freq>1),]$Var1# enregistrement des couples UCCS/UTS avec deus strates à la même prof d'apparition 
  #t_var[which(t_var$id_ucsuts%in%b & t_var$no_strate==1),c("no_strate","prof_appar_moy")]
  if(length(b)>0){
    d_var <- d_var[-which(d_var$id_ucsuts%in%b & d_var$no_strate==1),]
    }else{d_var <- d_var}#suppression de 250 horizon "O"
  rm(a);rm(b)
  
  #--------------------------------------------------------------------#
  #selection des variables (et des méthodes) quantitiatives sans NoData#
  #--------------------------------------------------------------------#
  #definition des formats
  d_var[,v_quanti] <- as.data.frame(lapply(d_var[,v_quanti],function(i)as.numeric(as.character(i))))
  #mise à jour du vecteur de variables quanti
  v_quanti2 <- v_quanti[which(apply(d_var[,v_quanti],2,sum,na.rm=T)!=0)]
    #si la somme est nulle alors on a que des NA
  d_var2 <- d_var[,-c(which(colnames(d_var)%in%c(v_quanti[which(apply(d_var[,v_quanti],2,sum,na.rm=T)==0)])))]
  

return(list(d_var2,v_quanti2))
}