dDepthWHyp = IF (aInclStrat EQ TRUE) THEN vDeltaWHyp + vDredDepthW - vTranHypEpiW ELSE 0.0 ENDIF ;
dNH4WHyp = ( wNTranNH4WHyp + wNAbioNH4WHyp + wNPrimNH4WHyp + wNBedNH4WHyp + wNWebNH4WHyp + uNBackLoadHyp / sDepthWHyp - aRelDeltaWHyp * oNH4WHyp - wNExchNH4Hyp - wNExchNH4WHyp  - wNAdvNH4W / sDepthWHyp ) * MassHyp;
dNO3WHyp = ( wNTranNO3WHyp + wNAbioNO3WHyp + wNPrimNO3WHyp + wNBedNO3WHyp + wNWebNO3WHyp - aRelDeltaWHyp * oNO3WHyp - wNExchNO3Hyp - wNExchNO3WHyp  - wNAdvNO3W / sDepthWHyp ) * MassHyp;
dPO4WHyp = ( wPTranPO4WHyp + wPAbioPO4WHyp + wPPrimPO4WHyp + wPBedPO4WHyp + wPWebPO4WHyp + uPBackLoadHyp / sDepthWHyp - aRelDeltaWHyp * oPO4WHyp - wPExchPO4Hyp - wPExchPO4WHyp  - wPAdvPO4W / sDepthWHyp ) * MassHyp;
dPAIMWHyp = ( wPTranAIMWHyp + wPAbioAIMWHyp - aRelDeltaWHyp * oPAIMWHyp - wPExchAIMHyp - wPExchAIMWHyp  - wPAdvAIMW / sDepthWHyp ) * MassHyp;
dSiO2WHyp = ( wSiTranSiO2Hyp + wSiAbioSiO2WHyp + wSiPrimSiO2WHyp - aRelDeltaWHyp * oSiO2WHyp - wSiExchSiO2Hyp - wSiExchSiO2WHyp  - wSiAdvSiO2W / sDepthWHyp ) * MassHyp;
dO2WHyp = ( wO2TranWHyp + wO2AbioWHyp + wO2PrimWHyp - tO2PrimSHyp + tO2BedWHyp / sDepthWHyp - aRelDeltaWHyp* oO2WHyp - wO2ExchHyp - wO2ExchWHyp  - wO2AdvW / sDepthWHyp ) * MassHyp;
dDDetWHyp = ( wDTranDetWHyp + wDAbioDetWHyp + wDPrimDetWHyp + wDBedDetWHyp + wDWebDetWHyp - aRelDeltaWHyp * oDDetWHyp - wDExchDetHyp - wDExchDetWHyp  - wDAdvDetW / sDepthWHyp ) * MassHyp;
dNDetWHyp = ( wNTranDetWHyp + wNAbioDetWHyp + wNPrimDetWHyp + wNBedDetWHyp + wNWebDetWHyp -aRelDeltaWHyp * oNDetWHyp - wNExchDetHyp - wNExchDetWHyp  - wNAdvDetW / sDepthWHyp ) * MassHyp;
dPDetWHyp = ( wPTranDetWHyp + wPAbioDetWHyp + wPPrimDetWHyp + wPBedDetWHyp + wPWebDetWHyp - aRelDeltaWHyp * oPDetWHyp - wPExchDetHyp - wPExchDetWHyp  - wPAdvDetW / sDepthWHyp ) * MassHyp;
dSiDetWHyp = ( wSiTranDetWHyp + wSiAbioDetWHyp + wSiPrimDetWHyp + wSiWebDetWHyp - aRelDeltaWHyp * oSiDetWHyp - wSiExchDetHyp - wSiExchDetWHyp  - wSiAdvDetW / sDepthWHyp ) * MassHyp;
dDIMWHyp = ( wDTranIMWHyp + wDAbioIMWHyp - aRelDeltaWHyp * oDIMWHyp - wDExchIMHyp - wDExchIMWHyp  - wDAdvIMW / sDepthWHyp ) * MassHyp;
dDDiatWHyp = ( wDTranDiatHyp + wDPrimDiatWHyp + wDWebDiatWHyp - aRelDeltaWHyp * oDDiatWHyp - wDExchDiatHyp - wDExchDiatWHyp  - wDAdvDiatW / sDepthWHyp ) * MassHyp;
dNDiatWHyp = ( wNTranDiatHyp + wNPrimDiatWHyp + wNWebDiatWHyp -aRelDeltaWHyp * oNDiatWHyp - wNExchDiatHyp - wNExchDiatWHyp  - wNAdvDiatW / sDepthWHyp ) * MassHyp;
dPDiatWHyp = ( wPTranDiatHyp + wPPrimDiatWHyp + wPWebDiatWHyp -aRelDeltaWHyp * oPDiatWHyp - wPExchDiatHyp - wPExchDiatWHyp  - wPAdvDiatW / sDepthWHyp ) * MassHyp;
dDGrenWHyp = ( wDTranGrenHyp + wDPrimGrenWHyp + wDWebGrenWHyp - aRelDeltaWHyp* oDGrenWHyp - wDExchGrenHyp - wDExchGrenWHyp  - wDAdvGrenW / sDepthWHyp ) * MassHyp;
dNGrenWHyp = ( wNTranGrenHyp + wNPrimGrenWHyp + wNWebGrenWHyp - aRelDeltaWHyp * oNGrenWHyp - wNExchGrenHyp - wNExchGrenWHyp  - wNAdvGrenW / sDepthWHyp ) * MassHyp;
dPGrenWHyp = ( wPTranGrenHyp + wPPrimGrenWHyp + wPWebGrenWHyp - aRelDeltaWHyp * oPGrenWHyp - wPExchGrenHyp - wPExchGrenWHyp  - wPAdvGrenW / sDepthWHyp ) * MassHyp;
dDBlueWHyp = ( wDTranBlueHyp + wDPrimBlueWHyp + wDWebBlueWHyp - aRelDeltaWHyp* oDBlueWHyp - wDExchBlueHyp - wDExchBlueWHyp  - wDAdvBlueW / sDepthWHyp ) * MassHyp;
dNBlueWHyp = ( wNTranBlueHyp + wNPrimBlueWHyp + wNWebBlueWHyp - aRelDeltaWHyp * oNBlueWHyp - wNExchBlueHyp - wNExchBlueWHyp  - wNAdvBlueW / sDepthWHyp ) * MassHyp;
dPBlueWHyp = ( wPTranBlueHyp + wPPrimBlueWHyp + wPWebBlueWHyp -aRelDeltaWHyp * oPBlueWHyp - wPExchBlueHyp - wPExchBlueWHyp  - wPAdvBlueW / sDepthWHyp ) * MassHyp;
dDZooHyp = ( wDTranZooHyp + wDWebZooHyp - aRelDeltaWHyp * oDZooHyp - wDExchZooHyp - wDExchZooWHyp  - wDAdvZooW / sDepthWHyp ) * MassHyp;
dNZooHyp = ( wNTranZooHyp + wNWebZooHyp -aRelDeltaWHyp * oNZooHyp - wNExchZooHyp - wNExchZooWHyp  - wNAdvZooW / sDepthWHyp ) * MassHyp;
dPZooHyp = ( wPTranZooHyp + wPWebZooHyp - aRelDeltaWHyp * oPZooHyp - wPExchZooHyp - wPExchZooWHyp  - wPAdvZooW / sDepthWHyp ) * MassHyp;
dDFiAd = tDWebFiAd;
dDFiJv = tDWebFiJv;
dNFiAd = tNWebFiAd;
dNFiJv = tNWebFiJv;
dPFiAd = tPWebFiAd;
dPFiJv = tPWebFiJv;
dDPisc = tDWebPisc;
dNH4S = tNAbioNH4S - tNBurNH4 + tNPrimNH4S + tNBedNH4S + tNWebNH4S;
dNO3S = tNAbioNO3S - tNBurNO3 + tNPrimNO3S + tNBedNO3S + tNWebNO3S;
dPO4S = tPAbioPO4S - tPBurPO4 + tPPrimPO4S + tPBedPO4S + tPWebPO4S;
dPAIMS = tPAbioAIMS - tPBurAIM - tPDredAIMS;
dDDetS = tDAbioDetS - tDBurDet + tDPrimDetS + tDBedDetSEpi + tDBedDetSHyp + tDWebDetS - tDDredDetS;
dNDetS = tNAbioDetS - tNBurDet + tNPrimDetS + tNBedDetS + tNWebDetS - tNDredDetS;
dPDetS = tPAbioDetS - tPBurDet + tPPrimDetS + tPBedDetS + tPWebDetS - tPDredDetS;
dSiDetS = tSiAbioDetS - tSiBurDet + tSiPrimDetS + tSiWebDetS - tSiDredDetS;
dDHumS = tDAbioHumS - tDBurHum - tDDredNetHumS;
dNHumS = tNAbioHumS - tNBurHum - tNDredNetHumS;
dPHumS = tPAbioHumS - tPBurHum - tPDredNetHumS;
dDIMS = tDAbioIMS - tDBurIM - tDDredNetIMS;
dDDiatS = tDPrimDiatS + tDWebDiatS - tDDredDiatS;
dNDiatS = tNPrimDiatS + tNWebDiatS - tNDredDiatS;
dPDiatS = tPPrimDiatS + tPWebDiatS - tPDredDiatS;
dDGrenS = tDPrimGrenS + tDWebGrenS - tDDredGrenS;
dNGrenS = tNPrimGrenS + tNWebGrenS - tPDredGrenS;
dPGrenS = tPPrimGrenS + tPWebGrenS - tPDredGrenS;
dDBlueS = tDPrimBlueS + tDWebBlueS - tDDredBlueS;
dNBlueS = tNPrimBlueS + tNWebBlueS - tNDredBlueS;
dPBlueS =tPPrimBlueS + tPWebBlueS - tPDredBlueS;
dDVeg = tDBedVeg - tDDredVeg;
dNVeg = tNBedVeg - tNDredVeg;
dPVeg = tPBedVeg - tPDredVeg;
dDBent = tDWebBent - tDDredBent;
dNBent = tNWebBent - tNDredBent;
dPBent = tPWebBent - tPDredBent;
dDepthWM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN vTranDepthW + vDeltaWM ELSE 0.0 ENDIF;
dNH4WM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN ( tNDifNH4M/sDepthWM - wNNitrWM + wNMinDetWM - tNEvNH4WM/sDepthWM - tNInfNH4WM/sDepthWM + wNExchNH4MEpi + wNExchNH4MHyp - aRelDeltaWM * oNH4WM + wNAdvNH4WM/sDepthWM) * MassWM ELSE 0.0 ENDIF;
dNO3WM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN ( tNDifNO3M/sDepthWM + wNNitrWM - wNDenitWM - tNEvNO3WM/sDepthWM - tNInfNO3WM/sDepthWM + wNExchNO3MEpi + wNExchNO3MHyp- aRelDeltaWM * oNO3WM + wNAdvNO3WM/sDepthWM) * MassWM ELSE 0.0 ENDIF;
dPO4WM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN ( - tPInfPO4WM / sDepthWM + tPDifPO4M / sDepthWM + wPMinDetWM - tPEvPO4WM / sDepthWM - wPSorpIMWM + wPExchPO4MEpi + wPExchPO4MHyp - aRelDeltaWM * oPO4WM + wPAdvPO4WM/sDepthWM) * MassWM ELSE 0.0 ENDIF;
dPAIMWM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN ( - tPSetAIMM / sDepthWM + wPSorpIMWM + wPExchAIMMEpi+ wPExchAIMMHyp - aRelDeltaWM * oPAIMWM + wPAdvAIMWM/sDepthWM) * MassWM ELSE 0.0 ENDIF;
dSiO2WM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN ( wSiMinDetWM + tSiMinDetSM / sDepthWM + wSiExchSiO2MEpi + wSiExchSiO2MHyp- aRelDeltaWM * oSiO2WM + wSiAdvSiO2WM/sDepthWM) * MassWM ELSE 0.0 ENDIF;
dO2WM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN ( wO2AbioM + wO2ExchMEpi + wO2ExchMHyp - aRelDeltaWM * oO2WM - tO2FlowPhra / sDepthWM + wO2AdvWM/sDepthWM) * MassWM ELSE 0.0 ENDIF;
dDDetWM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN ( tDMortShootPhra/sDepthWM - tDSetDetM/sDepthWM - wDMinDetWM + wDExchDetMEpi + wDExchDetMHyp- aRelDeltaWM * oDDetWM + wDAdvDetWM/sDepthWM) * MassWM ELSE 0.0 ENDIF;
dNDetWM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN ( tNMortShootPhra / sDepthWM - tNSetDetM / sDepthWM - wNMinDetWM + wNExchDetMEpi + wNExchDetMHyp - aRelDeltaWM * oNDetWM + wNAdvDetWM/sDepthWM) * MassWM ELSE 0.0 ENDIF;
dPDetWM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN ( tPMortShootPhra / sDepthWM - tPSetDetM / sDepthWM - wPMinDetWM + wPExchDetMEpi + wPExchDetMHyp - aRelDeltaWM * oPDetWM + wPAdvDetWM/sDepthWM) * MassWM ELSE 0.0 ENDIF;
dSiDetWM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN ( - tSiSetDetM / sDepthWM - wSiMinDetWM + wSiExchDetMEpi + wSiExchDetMHyp - aRelDeltaWM * oSiDetWM + wSiAdvDetWM/sDepthWM) * MassWM ELSE 0.0 ENDIF;
dDIMWM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN ( - tDSetIMM/sDepthWM + wDExchIMMEpi + wDExchIMMHyp - aRelDeltaWM * oDIMWM + wDAdvIMWM/sDepthWM) * MassWM ELSE 0.0 ENDIF;
dDDiatWM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN ( wDExchDiatMEpi +wDExchDiatMHyp - tDSetDiatM / sDepthWM - aRelDeltaWM * oDDiatWM + wDAdvDiatWM/sDepthWM) * MassWM ELSE 0.0 ENDIF;
dNDiatWM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN ( wNExchDiatMEpi + wNExchDiatMHyp - tNSetDiatM / sDepthWM - aRelDeltaWM * oNDiatWM + wNAdvDiatWM/sDepthWM) * MassWM ELSE 0.0 ENDIF;
dPDiatWM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN ( wPExchDiatMEpi +wPExchDiatMHyp - tPSetDiatM / sDepthWM - aRelDeltaWM * oPDiatWM + wPAdvDiatWM/sDepthWM) * MassWM ELSE 0.0 ENDIF;
dDGrenWM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN ( wDExchGrenMEpi + wDExchGrenMHyp- tDSetGrenM / sDepthWM - aRelDeltaWM * oDGrenWM + wDAdvGrenWM/sDepthWM) * MassWM ELSE 0.0 ENDIF;
dNGrenWM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN ( wNExchGrenMEpi + wNExchGrenMHyp - tNSetGrenM / sDepthWM - aRelDeltaWM * oNGrenWM + wNAdvGrenWM/sDepthWM) * MassWM ELSE 0.0 ENDIF;
dPGrenWM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN ( wPExchGrenMEpi + wPExchGrenMHyp - tPSetGrenM / sDepthWM - aRelDeltaWM * oPGrenWM + wPAdvGrenWM/sDepthWM) * MassWM ELSE 0.0 ENDIF;
dDBlueWM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN ( wDExchBlueMEpi + wDExchBlueMHyp- tDSetBlueM / sDepthWM - aRelDeltaWM * oDBlueWM + wDAdvBlueWM/sDepthWM) * MassWM ELSE 0.0 ENDIF;
dNBlueWM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN ( wNExchBlueMEpi + wNExchBlueMHyp - tNSetBlueM / sDepthWM - aRelDeltaWM * oNBlueWM + wNAdvBlueWM/sDepthWM) * MassWM ELSE 0.0 ENDIF;
dPBlueWM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN ( wPExchBlueMEpi + wPExchBlueMHyp - tPSetBlueM / sDepthWM - aRelDeltaWM * oPBlueWM + wPAdvBlueWM/sDepthWM) * MassWM ELSE 0.0 ENDIF;
dDZooM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN ( wDExchZooMEpi + wDExchZooMHyp - aRelDeltaWM * oDZooM + wDAdvZooWM/sDepthWM) * MassWM ELSE 0.0 ENDIF;
dNZooM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN ( wNExchZooMEpi + wNExchZooMHyp - aRelDeltaWM * oNZooM + wNAdvZooWM/sDepthWM) * MassWM ELSE 0.0 ENDIF;
dPZooM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN ( wPExchZooMEpi + wPExchZooMHyp - aRelDeltaWM * oPZooM + wPAdvZooWM/sDepthWM) * MassWM ELSE 0.0 ENDIF;
dNH4SM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN tNInfNH4WM - tNInfNH4SM +(1.0-fRefrDetS) * tNMinDetSM + tNMinHumSM - tNDifNH4M - tNDifGroundNH4M - tNNitrSM - tNBurNH4M - tNUptNH4PhraS + tNEvNH4WM ELSE 0.0 ENDIF;
dNO3SM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN tNInfNO3WM - tNInfNO3SM + tNNitrSM - tNDenitSM - tNDifNO3M - tNDifGroundNO3M - tNBurNO3M - tNUptNO3PhraS + tNEvNO3WM ELSE 0.0 ENDIF;
dPO4SM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN tPInfPO4WM - tPInfPO4SM + tPEvPO4WM +(1.0-fRefrDetS) * tPMinDetSM + tPMinHumSM - tPSorpIMSM - tPDifPO4M - tPDifGroundPO4M - tPChemPO4M - tPUptPhraS - tPBurPO4M ELSE 0.0 ENDIF;
dPAIMSM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN tPSetAIMM - tPBurAIMM + tPSorpIMSM ELSE 0.0 ENDIF;
dDDetSM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN tDMortRootPhra + tDSetDetM - tDMinDetSM + tDSetPhytM - tDBurDetM ELSE 0.0 ENDIF;
dNDetSM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN tNMortRootPhra + tNSetDetM - tNMinDetSM + tNSetPhytM - tNBurDetM ELSE 0.0 ENDIF;
dPDetSM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN tPMortRootPhra + tPSetDetM - tPMinDetSM + tPSetPhytM - tPBurDetM ELSE 0.0 ENDIF;
dSiDetSM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN tSiSetDetM - tSiMinDetSM + cSiDDiat * tDSetDiatM - tSiBurDetM ELSE 0.0 ENDIF;
dDHumSM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN fRefrDetS * tDMinDetSM - tDMinHumSM - tDBurHumM ELSE 0.0 ENDIF;
dNHumSM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN fRefrDetS * tNMinDetSM - tNMinHumSM - tNBurHumM ELSE 0.0 ENDIF;
dPHumSM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN fRefrDetS * tPMinDetSM - tPMinHumSM - tPBurHumM ELSE 0.0 ENDIF;
dDIMSM = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN tDSetIMM - tDBurIMM ELSE 0.0 ENDIF;
dDRootPhra = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN tDProdRootPhra - tDRespRootPhra - tDMortRootPhra - tDAllPhra + tDRealPhra ELSE 0.0 ENDIF;
dDShootPhra = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN tDProdShootPhra - tDRespShootPhra - tDMortShootPhra + tDAllPhra - tDRealPhra - tDManShootPhra ELSE 0.0 ENDIF;
dNRootPhra = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN tNUptRootPhra - tNMortRootPhra - tNTransPhra + tNRetrPhra ELSE 0.0 ENDIF;
dNShootPhra = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN tNUptShootPhra - tNMortShootPhra + tNTransPhra - tNRetrPhra - tNManShootPhra ELSE 0.0 ENDIF;
dPRootPhra = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN tPUptRootPhra - tPMortRootPhra - tPTransPhra + tPRetrPhra ELSE 0.0 ENDIF;
dPShootPhra = IF (InclMarsh EQ TRUE AND fMarsh GT NearZero) THEN tPUptShootPhra - tPMortShootPhra + tPTransPhra - tPRetrPhra - tPManShootPhra ELSE 0.0 ENDIF;
dDepthWEpi = IF (aInclStrat EQ TRUE) THEN vDeltaWEpi + vDeltaW + vTranHypEpiW ELSE vDeltaWEpi + vDeltaW + vDredDepthW ENDIF ;
dNH4WEpi = ( wNTranNH4WEpi + wNAbioNH4WEpi + wNPrimNH4WEpi + wNBedNH4WEpi + wNWebNH4WEpi + cNBackLoad / sDepthWEpi - aRelDeltaWEpi * oNH4WEpi - wNExchNH4Epi + wNExchNH4WEpi  + wNAdvNH4W / sDepthWEpi  - wNAdvNH4WM/sDepthWM * afVolMarshEpi) * MassEpi;
dNO3WEpi = ( wNTranNO3WEpi + wNAbioNO3WEpi + wNPrimNO3WEpi + wNBedNO3WEpi + wNWebNO3WEpi - aRelDeltaWEpi * oNO3WEpi - wNExchNO3Epi + wNExchNO3WEpi  + wNAdvNO3W / sDepthWEpi  - wNAdvNO3WM/sDepthWM * afVolMarshEpi) * MassEpi;
dPO4WEpi = ( wPTranPO4WEpi + wPAbioPO4WEpi + wPPrimPO4WEpi + wPBedPO4WEpi + wPWebPO4WEpi + cPBackLoad / sDepthWEpi - aRelDeltaWEpi * oPO4WEpi - wPExchPO4Epi + wPExchPO4WEpi  + wPAdvPO4W / sDepthWEpi  - wPAdvPO4WM/sDepthWM * afVolMarshEpi) * MassEpi;
dPAIMWEpi = ( wPTranAIMWEpi + wPAbioAIMWEpi - aRelDeltaWEpi * oPAIMWEpi - wPExchAIMEpi + wPExchAIMWEpi  + wPAdvAIMW / sDepthWEpi  - wPAdvAIMWM/sDepthWM * afVolMarshEpi) * MassEpi;
dSiO2WEpi = ( wSiTranSiO2Epi + wSiAbioSiO2WEpi + wSiPrimSiO2WEpi - aRelDeltaWEpi * oSiO2WEpi - wSiExchSiO2Epi + wSiExchSiO2WEpi  + wSiAdvSiO2W / sDepthWEpi  - wSiAdvSiO2WM/sDepthWM * afVolMarshEpi) * MassEpi;
dO2WEpi = ( wO2TranWEpi + wO2AbioWEpi + wO2PrimWEpi -tO2PrimSEpi + tO2BedWEpi / sDepthWEpi - aRelDeltaWEpi * oO2WEpi - wO2ExchEpi + wO2ExchWEpi  + wO2AdvW / sDepthWEpi  - wO2AdvWM/sDepthWM * afVolMarshEpi) * MassEpi;
dDDetWEpi = ( wDTranDetWEpi + wDAbioDetWEpi + wDPrimDetWEpi + wDBedDetWEpi + wDWebDetWEpi - aRelDeltaWEpi * oDDetWEpi - wDExchDetEpi + wDExchDetWEpi  + wDAdvDetW / sDepthWEpi  - wDAdvDetWM/sDepthWM * afVolMarshEpi) * MassEpi;
dNDetWEpi = ( wNTranDetWEpi + wNAbioDetWEpi + wNPrimDetWEpi + wNBedDetWEpi + wNWebDetWEpi - aRelDeltaWEpi * oNDetWEpi - wNExchDetEpi + wNExchDetWEpi  + wNAdvDetW / sDepthWEpi  - wNAdvDetWM/sDepthWM * afVolMarshEpi) * MassEpi;
dPDetWEpi = ( wPTranDetWEpi + wPAbioDetWEpi + wPPrimDetWEpi + wPBedDetWEpi + wPWebDetWEpi - aRelDeltaWEpi * oPDetWEpi - wPExchDetEpi + wPExchDetWEpi  + wPAdvDetW / sDepthWEpi  - wPAdvDetWM/sDepthWM * afVolMarshEpi) * MassEpi;
dSiDetWEpi = ( wSiTranDetWEpi + wSiAbioDetWEpi + wSiPrimDetWEpi + wSiWebDetWEpi - aRelDeltaWEpi * oSiDetWEpi - wSiExchDetEpi + wSiExchDetWEpi  + wSiAdvDetW / sDepthWEpi  - wSiAdvDetWM/sDepthWM * afVolMarshEpi) * MassEpi;
dDIMWEpi = ( wDTranIMWEpi + wDAbioIMWEpi - aRelDeltaWEpi * oDIMWEpi - wDExchIMEpi + wDExchIMWEpi  + wDAdvIMW / sDepthWEpi  - wDAdvIMWM/sDepthWM * afVolMarshEpi) * MassEpi;
dDDiatWEpi = ( wDTranDiatEpi + wDPrimDiatWEpi + wDWebDiatWEpi - aRelDeltaWEpi * oDDiatWEpi - wDExchDiatEpi + wDExchDiatWEpi  + wDAdvDiatW / sDepthWEpi  - wDAdvDiatWM/sDepthWM * afVolMarshEpi) * MassEpi;
dNDiatWEpi = ( wNTranDiatEpi + wNPrimDiatWEpi + wNWebDiatWEpi - aRelDeltaWEpi * oNDiatWEpi - wNExchDiatEpi + wNExchDiatWEpi  + wNAdvDiatW / sDepthWEpi  - wNAdvDiatWM/sDepthWM * afVolMarshEpi) * MassEpi;
dPDiatWEpi = ( wPTranDiatEpi + wPPrimDiatWEpi + wPWebDiatWEpi - aRelDeltaWEpi * oPDiatWEpi - wPExchDiatEpi + wPExchDiatWEpi  + wPAdvDiatW / sDepthWEpi  - wPAdvDiatWM/sDepthWM * afVolMarshEpi) * MassEpi;
dDGrenWEpi = ( wDTranGrenEpi + wDPrimGrenWEpi + wDWebGrenWEpi - aRelDeltaWEpi * oDGrenWEpi - wDExchGrenEpi + wDExchGrenWEpi  + wDAdvGrenW / sDepthWEpi  - wDAdvGrenWM/sDepthWM * afVolMarshEpi) * MassEpi;
dNGrenWEpi = ( wNTranGrenEpi + wNPrimGrenWEpi + wNWebGrenWEpi - aRelDeltaWEpi * oNGrenWEpi - wNExchGrenEpi + wNExchGrenWEpi  + wNAdvGrenW / sDepthWEpi  - wNAdvGrenWM/sDepthWM * afVolMarshEpi) * MassEpi;
dPGrenWEpi = ( wPTranGrenEpi + wPPrimGrenWEpi + wPWebGrenWEpi - aRelDeltaWEpi * oPGrenWEpi - wPExchGrenEpi + wPExchGrenWEpi  + wPAdvGrenW / sDepthWEpi  - wPAdvGrenWM/sDepthWM * afVolMarshEpi) * MassEpi;
dDBlueWEpi = ( wDTranBlueEpi + wDPrimBlueWEpi + wDWebBlueWEpi - aRelDeltaWEpi * oDBlueWEpi - wDExchBlueEpi + wDExchBlueWEpi  + wDAdvBlueW / sDepthWEpi  - wDAdvBlueWM/sDepthWM * afVolMarshEpi) * MassEpi;
dNBlueWEpi = ( wNTranBlueEpi + wNPrimBlueWEpi + wNWebBlueWEpi - aRelDeltaWEpi * oNBlueWEpi - wNExchBlueEpi + wNExchBlueWEpi  + wNAdvBlueW / sDepthWEpi  - wNAdvBlueWM/sDepthWM * afVolMarshEpi) * MassEpi;
dPBlueWEpi = ( wPTranBlueEpi + wPPrimBlueWEpi + wPWebBlueWEpi - aRelDeltaWEpi * oPBlueWEpi - wPExchBlueEpi + wPExchBlueWEpi  + wPAdvBlueW / sDepthWEpi  - wPAdvBlueWM/sDepthWM * afVolMarshEpi) * MassEpi;
dDZooEpi =  ( wDTranZooEpi + wDWebZooEpi - aRelDeltaWEpi * oDZooEpi - wDExchZooEpi + wDExchZooWEpi  + wDAdvZooW / sDepthWEpi  - wDAdvZooWM/sDepthWM * afVolMarshEpi) * MassEpi;
dNZooEpi =  ( wNTranZooEpi + wNWebZooEpi - aRelDeltaWEpi * oNZooEpi - wNExchZooEpi + wNExchZooWEpi  + wNAdvZooW / sDepthWEpi  - wNAdvZooWM/sDepthWM * afVolMarshEpi) * MassEpi;
dPZooEpi =  ( wPTranZooEpi + wPWebZooEpi - aRelDeltaWEpi * oPZooEpi - wPExchZooEpi + wPExchZooWEpi  + wPAdvZooW / sDepthWEpi  - wPAdvZooWM/sDepthWM * afVolMarshEpi) * MassEpi;
dDBlueSurf =tDTranBlueSurf + tDPrimBlueSurf + tDWebBlueSurf;
dNBlueSurf = tNTranBlueSurf + tNPrimBlueSurf + tNWebBlueSurf;
dPBlueSurf = tPTranBlueSurf + tPPrimBlueSurf + tPWebBlueSurf;
dDExtTotW = uDLoad - wDOutflTotEpi*sDepthWEpi - wDOutflTotHyp*sDepthWHyp + wDTranZooEpi * sDepthWEpi + wDTranZooHyp * sDepthWHyp + tDAbioTotW + tDPrimTotW + tDBedTotW + tDWebTotW - tDDredNetTotW + aDRelTotW + tDExchTotW + wDBedDetWEpi * sDepthWEpi + wDBedDetWHyp * sDepthWHyp;
dDExtTotWEpi = tDTranTotWEpi + tDAbioTotWEpi + tDPrimTotWEpi + wDBedDetWEpi * MassEpi + tDWebTotWEpi - aDRelTotWEpi - tDExchTotEpi + tDExchTotWEpi + tDAdvTotWEpi - tDAdvTotWMEpi;
dDExtTotWHyp = tDTranTotWHyp + tDAbioTotWHyp + tDPrimTotWHyp + wDBedDetWHyp * MassHyp + tDWebTotWHyp - aDRelTotWHyp - tDExchTotHyp - tDExchTotWHyp - tDAdvTotWHyp;
dNExtTotW = tNTranTotW + tNAbioTotW + tNPrimTotW + tNBedTotW + tNWebTotW - tNDredNetTotW - aNRelTotW - tNExchTotW;
dDExtTotS = - tDBurTot + tDAbioTotS + tDPrimTotS + tDWebTotS - tDDredNetTotS + tDBedTotS;
dNExtTotS = - tNBurTot + tNAbioTotS + tNPrimTotS + tNWebTotS - tNDredNetTotS + tNBedTotS;
dDExtTotT = uDLoad - wDOutflTotEpi*sDepthWEpi - tDDilBlueSurf - wDOutflTotHyp*sDepthWHyp + wDTranZooEpi * sDepthWEpi + wDTranZooHyp * sDepthWHyp - tDBurTot + tDAbioTotT + tDPrimTotT + tDBedTotT + tDWebTotT+ tDMarsTotT - tDDredNetTot - aDRelTotT;
dNExtTotT = uNLoad - wNOutflTotEpi * sDepthWEpi  - tNDilBlueSurf- wNOutflTotHyp * sDepthWHyp+ wNTranZooHyp * sDepthWHyp + wNTranZooEpi * sDepthWEpi + cNBackLoad - tNBurTot + tNAbioTotT + tNPrimTotT + tNBedTotT + tNWebTotT + tNMarsTotT - tNDredNetTot - aNRelTotT + tNUptBlueAirSurf + tNUptBlueAirEpi + tNUptBlueAirHyp;
dPExtTotT = uPLoad - wPOutflTotHyp * sDepthWHyp  - tPDilBlueSurf - wPOutflTotEpi * sDepthWEpi + wPTranZooHyp * sDepthWHyp + wPTranZooEpi * sDepthWEpi + cPBackLoad - tPBurTot + tPAbioTotT + tPPrimTotT + tPBedTotT + tPWebTotT + tPMarsTotT- tPDredNetTot - aPRelTotT;
dSiExtTotT = uSiLoad - wSiDilTotHyp*sDepthWHyp - wSiDilTotEpi*sDepthWEpi + tSiAbioTotT - tSiBurTot + tSiPrimTotT + tSiMarsTotT - tSiDredTot +tSiWebTotT - aSiRelTotT;
dO2ExtTotT = wO2TranWHyp * sDepthWHyp + wO2TranWEpi * sDepthWEpi + tO2BedWHyp + tO2BedWEpi + wO2PrimWHyp * sDepthWHyp + wO2PrimWEpi * sDepthWEpi + wO2AbioWHyp * sDepthWHyp + wO2AbioWEpi * sDepthWEpi + wO2AbioM* sDepthWM * fMarsh - tO2PrimS - tO2FlowPhra * fMarsh - aO2RelTotT;
