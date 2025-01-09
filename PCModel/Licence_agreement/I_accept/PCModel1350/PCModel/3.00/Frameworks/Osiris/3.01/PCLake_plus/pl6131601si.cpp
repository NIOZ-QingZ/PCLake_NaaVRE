_sMixDepthW0_ = _IF_ (_InclStrat_ _EQ_ _FALSE_) _THEN_ 0.0 _ELSEIF_ (_InitMixDepth_ _EQ_ _FALSE_) _THEN_ _cMixDepthW0_ _ELSEIF_ (_ReadMixDepth_ _EQ_ _TRUE_) _THEN_ _mMixDepth_ _ELSE_ _cMixDepth_ _ENDIF_;
_aInclStrat0_ =_IF_ ( _InclStrat_ _EQ_ _FALSE_) _THEN_ 0.0 _ELSEIF_ (_sMixDepthW0_ _GT_ _cDepthWInit0_ - _cMinDepthHypEpi_ ) _THEN_ 0.0 _ELSE_ 1.0 _ENDIF_;
_sDepthW0_ = _IF_ (_InitDepth_ _EQ_ _TRUE_) _THEN_ _cDepthWInit0_ _ELSE_  _cDepthW0_ _ENDIF_ ;
_MassHyp0_ = _IF_ ( _CalcMass_ _EQ_ _TRUE_  _AND_ _aInclStrat0_ _EQ_ _TRUE_ ) _THEN_ _sDepthW0_ - _sMixDepthW0_ _ELSE_ _cMinDepthHypEpi_ _ENDIF_ ;
_sNH4WHyp0_ = _cNH4WHyp0_ *_MassHyp0_;
_sNO3WHyp0_ = _cNO3WHyp0_ *_MassHyp0_;
_sPO4WHyp0_ = _cPO4WHyp0_ *_MassHyp0_;
_sPAIMWHyp0_ = _cPAIMWHyp0_ *_MassHyp0_;
_sSiO2WHyp0_ = _cSiO2WHyp0_ *_MassHyp0_;
_sO2WHyp0_ = _cO2WHyp0_ *_MassHyp0_;
_sDDetWHyp0_ = _cDDetWHyp0_ *_MassHyp0_;
_sNDetWHyp0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cNDDet0_ * _sDDetWHyp0_ _ELSE_ _cNDetWHyp0_ *_MassHyp0_ _ENDIF_;
_sPDetWHyp0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cPDDet0_ * _sDDetWHyp0_ _ELSE_ _cPDetWHyp0_ *_MassHyp0_ _ENDIF_;
_sSiDetWHyp0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cSiDDet0_ * _sDDetWHyp0_ _ELSE_ _cSiDetWHyp0_ *_MassHyp0_ _ENDIF_;
_sDIMWHyp0_ = _cDIMWHyp0_ *_MassHyp0_;
_sDDiatWHyp0_ = _cDDiatWHyp0_ *_MassHyp0_;
_sNDiatWHyp0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cNDDiat0_ * _sDDiatWHyp0_ _ELSE_ _cNDiatWHyp0_ *_MassHyp0_ _ENDIF_;
_sPDiatWHyp0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cPDDiat0_ * _sDDiatWHyp0_ _ELSE_ _cPDiatWHyp0_ *_MassHyp0_ _ENDIF_;
_sDGrenWHyp0_ = _cDGrenWHyp0_ *_MassHyp0_;
_sNGrenWHyp0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cNDGren0_ * _sDGrenWHyp0_ _ELSE_ _cNGrenWHyp0_ *_MassHyp0_ _ENDIF_;
_sPGrenWHyp0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cPDGren0_ * _sDGrenWHyp0_ _ELSE_ _cPGrenWHyp0_ *_MassHyp0_ _ENDIF_;
_sDBlueWHyp0_ = _cDBlueWHyp0_ *_MassHyp0_;
_sNBlueWHyp0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cNDBlue0_ * _sDBlueWHyp0_ _ELSE_ _cNBlueWHyp0_*_MassHyp0_ _ENDIF_;
_sPBlueWHyp0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cPDBlue0_ * _sDBlueWHyp0_ _ELSE_ _cPBlueWHyp0_ *_MassHyp0_ _ENDIF_;
_sDZooHyp0_ = _cDZooHyp0_ *_MassHyp0_;
_sNZooHyp0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cNDZooRef_ * _sDZooHyp0_ _ELSE_ _cNZooHyp0_ *_MassHyp0_ _ENDIF_;
_sPZooHyp0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cPDZooRef_ * _sDZooHyp0_ _ELSE_ _cPZooHyp0_ *_MassHyp0_ _ENDIF_;
_sDFiAd0_ = _cDFiAd0_;
_sDFiJv0_ = _cDFiJv0_;
_sNFiAd0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cNDFishRef_ * _sDFiAd0_ _ELSE_ _cNFiAd0_ _ENDIF_;
_sNFiJv0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cNDFishRef_ * _sDFiJv0_ _ELSE_ _cNFiJv0_ _ENDIF_;
_sPFiAd0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cPDFishRef_ * _sDFiAd0_ _ELSE_ _cPFiAd0_ _ENDIF_;
_sPFiJv0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cPDFishRef_ * _sDFiJv0_ _ELSE_ _cPFiJv0_ _ENDIF_;
_sDPisc0_ = _cDPisc0_;
_MassEpi0_ = _IF_ (_CalcMass_ _EQ_ _FALSE_) _THEN_ 1.0 _ELSEIF_ (_InclStrat_ _EQ_ _FALSE_) _THEN_ _sDepthW0_ _ELSEIF_ (_sDepthW0_  _LT_ _sMixDepthW0_) _THEN_ _sDepthW0_ _ELSE_ _sMixDepthW0_ _ENDIF_;
_sNH4WEpi0_ = _cNH4WEpi0_ * _MassEpi0_;
_sNO3WEpi0_ = _cNO3WEpi0_ *_MassEpi0_;
_sPO4WEpi0_ = _cPO4WEpi0_ *_MassEpi0_;
_sPAIMWEpi0_ = _cPAIMWEpi0_ *_MassEpi0_;
_sSiO2WEpi0_ = _cSiO2WEpi0_ * _MassEpi0_;
_sO2WEpi0_ = _cO2WEpi0_ *_MassEpi0_;
_sDDetWEpi0_ = _cDDetWEpi0_ *_MassEpi0_;
_sNDetWEpi0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cNDDet0_ * _sDDetWEpi0_ _ELSE_ _cNDetWEpi0_ *_MassEpi0_ _ENDIF_ ;
_sPDetWEpi0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cPDDet0_ * _sDDetWEpi0_ _ELSE_ _cPDetWEpi0_ *_MassEpi0_ _ENDIF_;
_sSiDetWEpi0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cSiDDet0_ * _sDDetWEpi0_ _ELSE_ _cSiDetWEpi0_ *_MassEpi0_ _ENDIF_;
_sDIMWEpi0_ = _cDIMWEpi0_ *_MassEpi0_;
_sDDiatWEpi0_ = _cDDiatWEpi0_ *_MassEpi0_;
_sNDiatWEpi0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cNDDiat0_ * _sDDiatWEpi0_ _ELSE_ _cNDiatWEpi0_ *_MassEpi0_ _ENDIF_;
_sPDiatWEpi0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cPDDiat0_ * _sDDiatWEpi0_ _ELSE_ _cPDiatWEpi0_ *_MassEpi0_ _ENDIF_;
_sDGrenWEpi0_ = _cDGrenWEpi0_ *_MassEpi0_;
_sNGrenWEpi0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cNDGren0_ * _sDGrenWEpi0_ _ELSE_ _cNGrenWEpi0_ *_MassEpi0_ _ENDIF_;
_sPGrenWEpi0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cPDGren0_ * _sDGrenWEpi0_ _ELSE_ _cPGrenWEpi0_ *_MassEpi0_ _ENDIF_;
_sDBlueWEpi0_ = _cDBlueWEpi0_ *_MassEpi0_;
_sNBlueWEpi0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cNDBlue0_ * _sDBlueWEpi0_ _ELSE_ _cNBlueWEpi0_ *_MassEpi0_ _ENDIF_;
_sPBlueWEpi0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cPDBlue0_ * _sDBlueWEpi0_ _ELSE_ _cPBlueWEpi0_ *_MassEpi0_ _ENDIF_;
_sDZooEpi0_ = _cDZooEpi0_ *_MassEpi0_;
_sNZooEpi0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cNDZooRef_ * _sDZooEpi0_ _ELSE_ _cNZooEpi0_ *_MassEpi0_ _ENDIF_;
_sPZooEpi0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cPDZooRef_ * _sDZooEpi0_ _ELSE_ _cPZooEpi0_ *_MassEpi0_ _ENDIF_;
_sDBlueSurf0_ = _IF_ (_InclSurf_ _EQ_ _TRUE_) _THEN_ _cDBlueSurf0_ _ELSE_ 0.0 _ENDIF_;
_sNBlueSurf0_ = _IF_ (_InclSurf_ _EQ_ _TRUE_) _THEN_ _cNBlueSurf0_ _ELSE_ 0.0 _ENDIF_;
_sPBlueSurf0_ = _IF_ (_InclSurf_ _EQ_ _TRUE_) _THEN_ _cPBlueSurf0_ _ELSE_ 0.0 _ENDIF_;
_sNH4S0_ = _cNH4S0_;
_sNO3S0_ = _cNO3S0_;
_bRhoSolidS0_ = _fDOrgS0_ * _cRhoOM_ + (1.0 - _fDOrgS0_) * _cRhoIM_;
_bPorS0_ = (1.0 - _fDTotS0_) * _bRhoSolidS0_ / _cRhoWat_ / ( _fDTotS0_ + (1.0 - _fDTotS0_) * _bRhoSolidS0_ / _cRhoWat_ );
_bRhoTotS0_ = _bRhoSolidS0_ * (1.0 - _bPorS0_);
_bDTotS0_ = _bRhoTotS0_ * _cDepthS_;
_sPO4S0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ (1.0 - _fPAdsS0_) * _fPInorgS0_ * _bDTotS0_ _ELSE_ _cPO4S0_ _ENDIF_;
_sPAIMS0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _fPAdsS0_ * _fPInorgS0_ * _bDTotS0_ _ELSE_ _cPAIMS0_ _ENDIF_;
_sDDetS0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _fDDetS0_ * _fDOrgS0_ * _bDTotS0_ _ELSE_ _cDDetS0_ _ENDIF_;
_sNDetS0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cNDDet0_ * _sDDetS0_ _ELSE_ _cNDetS0_ _ENDIF_;
_sPDetS0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cPDDet0_ * _sDDetS0_ _ELSE_ _cPDetS0_ _ENDIF_;
_sSiDetS0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cSiDDet0_ * _sDDetS0_ _ELSE_ _cSiDetS0_ _ENDIF_;
_sDHumS0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ (1.0 - _fDDetS0_) * _fDOrgS0_ * _bDTotS0_ _ELSE_ _cDHumS0_ _ENDIF_;
_sNHumS0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cNDHum0_ * _sDHumS0_ _ELSE_ _cNHumS0_ _ENDIF_;
_sPHumS0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cPDHum0_ * _sDHumS0_ _ELSE_ _cPHumS0_ _ENDIF_;
_sDIMS0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _bDTotS0_ - _sDHumS0_ - _sDDetS0_ _ELSE_ _cDIMS0_ _ENDIF_;
_sDDiatS0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _fSedPhyt0_ * _sDDiatWHyp0_ _ELSE_ _cDDiatS0_ _ENDIF_;
_sNDiatS0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cNDDiat0_ * _sDDiatS0_ _ELSE_ _cNDiatS0_ _ENDIF_;
_sPDiatS0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cPDDiat0_ * _sDDiatS0_ _ELSE_ _cPDiatS0_ _ENDIF_;
_sDGrenS0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _fSedPhyt0_ * _sDGrenWHyp0_ _ELSE_ _cDGrenS0_ _ENDIF_;
_sNGrenS0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cNDGren0_ * _sDGrenS0_ _ELSE_ _cNGrenS0_ _ENDIF_;
_sPGrenS0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cPDGren0_ * _sDGrenS0_ _ELSE_ _cPGrenS0_ _ENDIF_;
_sDBlueS0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _fSedPhyt0_ * _sDBlueWHyp0_ _ELSE_ _cDBlueS0_ _ENDIF_;
_sNBlueS0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cNDBlue0_ * _sDBlueS0_ _ELSE_ _cNBlueS0_ _ENDIF_;
_sPBlueS0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cPDBlue0_ * _sDBlueS0_ _ELSE_ _cPBlueS0_ _ENDIF_;
_sDVeg0_ = _cDVeg0_;
_sNVeg0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cNDVeg0_ * _sDVeg0_ _ELSE_ _cNVeg0_ _ENDIF_;
_sPVeg0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cPDVeg0_ * _sDVeg0_ _ELSE_ _cPVeg0_ _ENDIF_;
_sVegHeight0_ = _MIN_ (_cVegHeight_, _sDepthW0_ );
_sDBent0_ = _cDBent0_;
_sNBent0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cNDBentRef_ * _sDBent0_ _ELSE_ _cNBent0_ _ENDIF_;
_sPBent0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cPDBentRef_ * _sDBent0_ _ELSE_ _cPBent0_ _ENDIF_;
_sDepthWM0_ = _cDepthWM0_;
_MassWM0_ = _IF_ _CalcMass_ _EQ_ _TRUE_ _THEN_ _sDepthWM0_ _ELSE_ 1.0 _ENDIF_;
_sNH4WM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _sNH4WEpi0_ _ELSE_ _cNH4WM0_ * _MassWM0_ _ENDIF_;
_sNO3WM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _sNO3WEpi0_ _ELSE_ _cNO3WM0_ * _MassWM0_ _ENDIF_;
_sPO4WM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _sPO4WEpi0_ _ELSE_ _cPO4WM0_ * _MassWM0_ _ENDIF_;
_sPAIMWM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _sPAIMWEpi0_ _ELSE_ _cPAIMWM0_ * _MassWM0_ _ENDIF_;
_sSiO2WM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _sSiO2WEpi0_ _ELSE_ _cSiO2WM0_ * _MassWM0_ _ENDIF_;
_sO2WM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _sO2WEpi0_ _ELSE_ _cO2WM0_ * _MassWM0_ _ENDIF_;
_sDDetWM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _sDDetWEpi0_ _ELSE_ _cDDetWM0_ * _MassWM0_ _ENDIF_;
_sNDetWM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _sNDetWEpi0_ _ELSE_ _cNDetWM0_ * _MassWM0_ _ENDIF_;
_sPDetWM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _sPDetWEpi0_ _ELSE_ _cPDetWM0_ * _MassWM0_ _ENDIF_;
_sSiDetWM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _sSiDetWEpi0_ _ELSE_ _cSiDetWM0_ * _MassWM0_ _ENDIF_;
_sDIMWM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _sDIMWEpi0_ _ELSE_ _cDIMWM0_ * _MassWM0_ _ENDIF_;
_sDDiatWM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _sDDiatWEpi0_ _ELSE_ _cDDiatWM0_ * _MassWM0_ _ENDIF_;
_sNDiatWM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _sNDiatWEpi0_ _ELSE_ _cNDiatWM0_ * _MassWM0_ _ENDIF_;
_sPDiatWM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _sPDiatWEpi0_ _ELSE_ _cPDiatWM0_ * _MassWM0_ _ENDIF_;
_sDGrenWM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _sDGrenWEpi0_ _ELSE_ _cDGrenWM0_ * _MassWM0_ _ENDIF_;
_sNGrenWM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _sNGrenWEpi0_ _ELSE_ _cNGrenWM0_ * _MassWM0_ _ENDIF_;
_sPGrenWM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _sPGrenWEpi0_ _ELSE_ _cPGrenWM0_ * _MassWM0_ _ENDIF_;
_sDBlueWM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _sDBlueWEpi0_ _ELSE_ _cDBlueWM0_ * _MassWM0_ _ENDIF_;
_sNBlueWM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _sNBlueWEpi0_ _ELSE_ _cNBlueWM0_ * _MassWM0_ _ENDIF_;
_sPBlueWM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _sPBlueWEpi0_ _ELSE_ _cPBlueWM0_ * _MassWM0_ _ENDIF_;
_sDZooM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _sDZooEpi0_ _ELSE_ _cDZooM0_ * _MassWM0_ _ENDIF_;
_sNZooM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _sNZooEpi0_ _ELSE_ _cNZooM0_ * _MassWM0_ _ENDIF_;
_sPZooM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _sPZooEpi0_ _ELSE_ _cPZooM0_ * _MassWM0_ _ENDIF_;
_sNH4SM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _sNH4S0_ _ELSE_ _cNH4SM0_ _ENDIF_;
_sNO3SM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _sNO3S0_ _ELSE_ _cNO3SM0_ _ENDIF_;
_bRhoSolidSM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _fDOrgSM0_ * _cRhoOM_ +(1.0 - _fDOrgSM0_) * _cRhoIM_ _ELSE_ _fDOrgSM0_ * _cRhoOM_ +(1.0 - _fDOrgSM0_) * _cRhoIM_ _ENDIF_;
_bPorSM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ (1.0 - _fDTotSM0_) * _bRhoSolidSM0_ / _cRhoWat_ /(_fDTotSM0_ + (1.0 - _fDTotSM0_) * _bRhoSolidSM0_ / _cRhoWat_) _ELSE_ (1.0 - _fDTotSM0_) * _bRhoSolidSM0_ / _cRhoWat_ /(_fDTotSM0_ + (1.0 - _fDTotSM0_) * _bRhoSolidSM0_ / _cRhoWat_) _ENDIF_;
_bRhoTotSM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _bRhoSolidSM0_ * (1.0 - _bPorSM0_) _ELSE_ 0.0 _ENDIF_;
_bDTotSM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _bRhoTotSM0_ * _cDepthSM_ _ELSE_ 0.0 _ENDIF_;
_fPAdsSM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _fPAdsS0_ _ELSE_ 0.0 _ENDIF_;
_sPO4SM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ (1.0 - _fPAdsSM0_) * _fPInorgSM0_ * _bDTotSM0_ _ELSE_ _cPO4SM0_ _ENDIF_;
_sPAIMSM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _fPAdsSM0_ * _fPInorgSM0_ * _bDTotSM0_ _ELSE_ _cPAIMSM0_ _ENDIF_;
_sDDetSM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _fDDetSM0_ * _fDOrgSM0_ * _bDTotSM0_ _ELSE_ _cDDetSM0_ _ENDIF_;
_sNDetSM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cNDDet0_ * _sDDetSM0_ _ELSE_ _cNDetSM0_ _ENDIF_;
_sPDetSM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cPDDet0_ * _sDDetSM0_ _ELSE_ _cPDetSM0_ _ENDIF_;
_sSiDetSM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cSiDDet0_ * _sDDetSM0_ _ELSE_ _cSiDetSM0_ _ENDIF_;
_sDHumSM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ (1.0 - _fDDetSM0_) * _fDOrgSM0_ * _bDTotSM0_ _ELSE_ _cDHumSM0_ _ENDIF_;
_sNHumSM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cNDHum0_ * _sDHumSM0_ _ELSE_ _cNHumSM0_ _ENDIF_;
_sPHumSM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cPDHum0_ * _sDHumSM0_ _ELSE_ _cPHumSM0_ _ENDIF_;
_sDIMSM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _bDTotSM0_ - _sDHumSM0_ - _sDDetSM0_ _ELSE_ _cDIMSM0_ _ENDIF_;
_sDRootPhra0_ = _cDRootPhra0_;
_sDShootPhra0_ = _cDShootPhra0_;
_sNRootPhra0_ = _IF_ (_FALSE_ _EQ_ _InclMarsh_ _OR_ _fMarsh_ _LE_ _NearZero_) _THEN_ _cNRootPhra0_ _ELSE_ _cNDPhra0_ * _sDRootPhra0_ _ENDIF_;
_sNShootPhra0_ = _IF_ (_FALSE_ _EQ_ _InclMarsh_ _OR_ _fMarsh_ _LE_ _NearZero_) _THEN_ _cNShootPhra0_ _ELSE_ _cNDPhra0_ * _sDShootPhra0_ _ENDIF_;
_sPRootPhra0_ = _IF_ (_FALSE_ _EQ_ _InclMarsh_ _OR_ _fMarsh_ _LE_ _NearZero_) _THEN_ _cPRootPhra0_ _ELSE_ _cPDPhra0_ * _sDRootPhra0_ _ENDIF_;
_sPShootPhra0_ = _IF_ (_FALSE_ _EQ_ _InclMarsh_ _OR_ _fMarsh_ _LE_ _NearZero_) _THEN_ _cPShootPhra0_ _ELSE_ _cPDPhra0_ * _sDShootPhra0_ _ENDIF_;
_uDPhytWHyp0_ = _sDDiatWHyp0_ + _sDGrenWHyp0_ + _sDBlueWHyp0_;
_uDPhytWEpi0_ = _sDDiatWEpi0_ + _sDGrenWEpi0_ + _sDBlueWEpi0_;
_uPPhytWHyp0_ = _sPDiatWHyp0_ + _sPGrenWHyp0_ + _sPBlueWHyp0_;
_uPPhytWEpi0_ = _sPDiatWEpi0_ + _sPGrenWEpi0_ + _sPBlueWEpi0_;
_uNPhytWHyp0_ = _sNDiatWHyp0_ + _sNGrenWHyp0_ + _sNBlueWHyp0_;
_uNPhytWEpi0_ = _sNDiatWEpi0_ + _sNGrenWEpi0_ + _sNBlueWEpi0_;
_uDPhytS0_ = _sDDiatS0_ + _sDGrenS0_ + _sDBlueS0_;
_uPPhytS0_ = _sPDiatS0_ + _sPGrenS0_ + _sPBlueS0_;
_uNPhytS0_ = _sNDiatS0_ + _sNGrenS0_ + _sNBlueS0_;
_uDPhytWM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _uDPhytWEpi0_ _ELSE_ _sDGrenWM0_ + _sDDiatWM0_ + _sDBlueWM0_ _ENDIF_;
_uPPhytWM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _uPPhytWEpi0_ _ELSE_ _sPGrenWM0_ + _sPDiatWM0_ + _sPBlueWM0_ _ENDIF_;
_uNPhytWM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _uNPhytWEpi0_ _ELSE_ _sNGrenWM0_ + _sNDiatWM0_ + _sNBlueWM0_ _ENDIF_;
_uSiDiatWM0_ = _IF_ (_InitCalc_ _EQ_ _TRUE_) _THEN_ _cSiDDiat_ * _sDDiatWEpi0_ _ELSE_ _cSiDDiat_ * _sDDiatWM0_ _ENDIF_;
_uDVeg0_ = _sDVeg0_;
_uPVeg0_ = _sPVeg0_;
_uNVeg0_ = _sNVeg0_;
_uDFish0_ = _sDFiJv0_ + _sDFiAd0_;
_uPFish0_ = _sPFiJv0_ + _sPFiAd0_;
_uNFish0_ = _sNFiJv0_ + _sNFiAd0_;
_uPPisc0_ = _cPDPisc_ * _sDPisc0_;
_uNPisc0_ = _cNDPisc_ * _sDPisc0_;
_uO2TotTHyp0_ = _IF_ (_aInclStrat0_ _EQ_ _TRUE_) _THEN_ (_sO2WHyp0_ ) * (_sDepthW0_ - _sMixDepthW0_) / _MassHyp0_ _ELSE_ 0.0 _ENDIF_;
_uDTotTHyp0_ = _IF_ (_aInclStrat0_ _EQ_ _TRUE_) _THEN_ (_sDIMWHyp0_ + _sDDetWHyp0_ + _uDPhytWHyp0_ + _sDZooHyp0_) * (_sDepthW0_ - _sMixDepthW0_) / _MassHyp0_ _ELSE_ 0.0 _ENDIF_;
_uPTotTHyp0_ = _IF_ (_aInclStrat0_ _EQ_ _TRUE_) _THEN_ (_sPO4WHyp0_ + _sPDetWHyp0_ + _sPAIMWHyp0_ + _uPPhytWHyp0_ + _sPZooHyp0_) * (_sDepthW0_ - _sMixDepthW0_) / _MassHyp0_ _ELSE_ 0.0 _ENDIF_;
_uNTotTHyp0_ = _IF_ (_aInclStrat0_ _EQ_ _TRUE_) _THEN_ (_sNH4WHyp0_ + _sNO3WHyp0_ + _sNDetWHyp0_ + _uNPhytWHyp0_ + _sNZooHyp0_) * (_sDepthW0_ - _sMixDepthW0_) / _MassHyp0_ _ELSE_ 0.0 _ENDIF_ ;
_uSiTotTHyp0_ = _IF_ (_aInclStrat0_ _EQ_ _TRUE_) _THEN_ (_sSiO2WHyp0_ + _sSiDetWHyp0_ + _cSiDDiat_ * _sDDiatWHyp0_) * (_sDepthW0_ - _sMixDepthW0_) / _MassHyp0_ _ELSE_ 0.0 _ENDIF_;
_uDTotTSurf0_ = _IF_ (_InclSurf_ _EQ_ _TRUE_) _THEN_ _sDBlueSurf0_ _ELSE_ 0.0 _ENDIF_;
_uPTotTSurf0_ = _IF_ (_InclSurf_ _EQ_ _TRUE_) _THEN_ _sPBlueSurf0_ _ELSE_ 0.0 _ENDIF_;
_uNTotTSurf0_ = _IF_ (_InclSurf_ _EQ_ _TRUE_) _THEN_ _sNBlueSurf0_ _ELSE_ 0.0 _ENDIF_;
_uDTotM0_ = _IF_ (_FALSE_ _EQ_ _InclMarsh_ _OR_ _fMarsh_ _LE_ _NearZero_) _THEN_ 0.0 _ELSE_ ((_sDIMWM0_ + _sDDetWM0_ + _uDPhytWM0_ + _sDZooM0_) * _sDepthWM0_ / _MassWM0_ + _sDIMSM0_ + _sDHumSM0_ + _sDDetSM0_ + _sDShootPhra0_ + _sDRootPhra0_) * _fMarsh_ _ENDIF_;
_uPTotM0_ = _IF_ (_FALSE_ _EQ_ _InclMarsh_ _OR_ _fMarsh_ _LE_ _NearZero_) _THEN_ 0.0 _ELSE_ ((_sPO4WM0_ + _sPDetWM0_ + _sPAIMWM0_ + _uPPhytWM0_ + _sPZooM0_) * _sDepthWM0_ / _MassWM0_ + _sPO4SM0_ + _sPDetSM0_ + _sPHumSM0_ + _sPAIMSM0_ + _sPShootPhra0_ + _sPRootPhra0_) * _fMarsh_ _ENDIF_;
_uNTotM0_ = _IF_ (_FALSE_ _EQ_ _InclMarsh_ _OR_ _fMarsh_ _LE_ _NearZero_) _THEN_ 0.0 _ELSE_ ((_sNH4WM0_ + _sNO3WM0_ + _sNDetWM0_ + _uNPhytWM0_ + _sNZooM0_) * _sDepthWM0_ / _MassWM0_ + _sNH4SM0_ + _sNO3SM0_ + _sNHumSM0_ + _sNDetSM0_ + _sNShootPhra0_ + _sNRootPhra0_) * _fMarsh_ _ENDIF_;
_uSiTotM0_ = _IF_ (_FALSE_ _EQ_ _InclMarsh_ _OR_ _fMarsh_ _LE_ _NearZero_) _THEN_ 0.0 _ELSE_ ((_sSiO2WM0_ + _sSiDetWM0_ + _uSiDiatWM0_) * _sDepthWM0_ / _MassWM0_ + _sSiDetSM0_) * _fMarsh_ _ENDIF_;
_uO2TotM0_ = _IF_ (_FALSE_ _EQ_ _InclMarsh_ _OR_ _fMarsh_ _LE_ _NearZero_) _THEN_ 0.0 _ELSE_ ((_sO2WM0_) * _sDepthWM0_ / _MassWM0_ ) * _fMarsh_ _ENDIF_;
_uO2TotT0_ = _IF_ (_aInclStrat0_ _EQ_ _TRUE_) _OR_ ((_aInclStrat0_ _EQ_ _FALSE_) _AND_ (_sDepthW0_ _GE_ _sMixDepthW0_)) _THEN_ _uO2TotTHyp0_ + ( _sO2WEpi0_) * _sMixDepthW0_ / _MassEpi0_ + _uO2TotM0_  _ELSE_ ( _sO2WEpi0_) *_sDepthW0_ / _MassEpi0_ + _uO2TotM0_  _ENDIF_ ;
_uDTotT0_ = _IF_ (_aInclStrat0_ _EQ_ _TRUE_) _OR_ ((_aInclStrat0_ _EQ_ _FALSE_) _AND_ (_sDepthW0_ _GE_ _sMixDepthW0_)) _THEN_ _uDTotTHyp0_ + (_sDIMWEpi0_ + _sDDetWEpi0_ + _uDPhytWEpi0_ + _sDZooEpi0_) * _sMixDepthW0_ / _MassEpi0_ + _uDVeg0_ + _sDIMS0_ + _sDHumS0_ + _sDDetS0_ + _uDPhytS0_ + _uDFish0_ + _sDPisc0_ + _sDBent0_ + _uDTotM0_ + _uDTotTSurf0_ _ELSE_ (_sDIMWEpi0_ + _sDDetWEpi0_ + _uDPhytWEpi0_ + _sDZooEpi0_) * _sDepthW0_ / _MassEpi0_ + _uDVeg0_ + _sDIMS0_ + _sDHumS0_ + _sDDetS0_ + _uDPhytS0_ + _uDFish0_ + _sDPisc0_ + _sDBent0_ + _uDTotM0_ + _uDTotTSurf0_ _ENDIF_ ;
_uPTotT0_ = _IF_ (_aInclStrat0_ _EQ_ _TRUE_) _OR_ ((_aInclStrat0_ _EQ_ _FALSE_) _AND_ (_sDepthW0_ _GE_ _sMixDepthW0_)) _THEN_ _uPTotTHyp0_ + (_sPO4WEpi0_ + _sPDetWEpi0_ + _sPAIMWEpi0_ + _uPPhytWEpi0_ + _sPZooEpi0_) * _sMixDepthW0_ / _MassEpi0_+ _uPVeg0_ + _sPO4S0_ + _sPDetS0_ + _sPAIMS0_ + _sPHumS0_ + _uPPhytS0_ + _uPFish0_ + _uPPisc0_ + _sPBent0_ + _uPTotM0_  + _uPTotTSurf0_ _ELSE_ + (_sPO4WEpi0_ + _sPDetWEpi0_ + _sPAIMWEpi0_ + _uPPhytWEpi0_ + _sPZooEpi0_) * _sDepthW0_ / _MassEpi0_+ _uPVeg0_ + _sPO4S0_ + _sPDetS0_ + _sPAIMS0_ + _sPHumS0_ + _uPPhytS0_ + _uPFish0_ + _uPPisc0_ + _sPBent0_ + _uPTotM0_  + _uPTotTSurf0_ _ENDIF_  ;
_uNTotT0_ =  _IF_ (_aInclStrat0_ _EQ_ _TRUE_) _OR_ ((_aInclStrat0_ _EQ_ _FALSE_) _AND_ (_sDepthW0_ _GE_ _sMixDepthW0_)) _THEN_  _uNTotTHyp0_ + (_sNH4WEpi0_ + _sNO3WEpi0_ + _sNDetWEpi0_ + _uNPhytWEpi0_ + _sNZooEpi0_) * _sMixDepthW0_ / _MassEpi0_ + _uNVeg0_ + _sNH4S0_ + _sNO3S0_ + _sNDetS0_ + _uNPhytS0_ + _sNHumS0_ + _uNFish0_ + _uNPisc0_ + _sNBent0_ + _uNTotM0_  + _uNTotTSurf0_ _ELSE_   (_sNH4WEpi0_ + _sNO3WEpi0_ + _sNDetWEpi0_ + _uNPhytWEpi0_ + _sNZooEpi0_) * _sDepthW0_ / _MassEpi0_ + _uNVeg0_ + _sNH4S0_ + _sNO3S0_ + _sNDetS0_ + _uNPhytS0_ + _sNHumS0_ + _uNFish0_ + _uNPisc0_ + _sNBent0_ + _uNTotM0_  + _uNTotTSurf0_ _ENDIF_;
_uSiTotT0_ = _IF_ (_aInclStrat0_ _EQ_ _TRUE_) _OR_ ((_aInclStrat0_ _EQ_ _FALSE_) _AND_ (_sDepthW0_ _GE_ _sMixDepthW0_)) _THEN_ _uSiTotTHyp0_+ (_sSiO2WEpi0_ + _sSiDetWEpi0_ + _cSiDDiat_ * _sDDiatWEpi0_) * _sMixDepthW0_ / _MassEpi0_ + _sSiDetS0_ + _cSiDDiat_*_sDDiatS0_ + _uSiTotM0_  _ELSE_ (_sSiO2WEpi0_ + _sSiDetWEpi0_ + _cSiDDiat_ * _sDDiatWEpi0_) * _sDepthW0_ / _MassEpi0_ + _sSiDetS0_ + _cSiDDiat_*_sDDiatS0_ + _uSiTotM0_  _ENDIF_ ;
_sDExtTotT0_ = _uDTotT0_;
_sNExtTotT0_ = _uNTotT0_;
_sPExtTotT0_ = _uPTotT0_;
_sSiExtTotT0_ = _uSiTotT0_;
_sO2ExtTotT0_ = _uO2TotT0_;
_sDepthExtTotT0_ = _sDepthW0_;
