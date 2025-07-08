****** DO CASE for the XMLDemo.xml file structure ********
DO CASE
	CASE cKey == "cfdi:Comprobante" .AND. cPath == ""
		
		IF hb_HHasKey( hAttrib, "xmlns:cfdi" )
			xAttribValue := hAttrib ["xmlns:cfdi"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "xmlns:xsi" )
			xAttribValue := hAttrib ["xmlns:xsi"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "xsi:schemaLocation" )
			xAttribValue := hAttrib ["xsi:schemaLocation"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "Version" )
			xAttribValue := hAttrib ["Version"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "Serie" )
			xAttribValue := hAttrib ["Serie"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "Folio" )
			xAttribValue := hAttrib ["Folio"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "Fecha" )
			xAttribValue := hAttrib ["Fecha"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "Sello" )
			xAttribValue := hAttrib ["Sello"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "FormaPago" )
			xAttribValue := hAttrib ["FormaPago"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "NoCertificado" )
			xAttribValue := hAttrib ["NoCertificado"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "Certificado" )
			xAttribValue := hAttrib ["Certificado"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "CondicionesDePago" )
			xAttribValue := hAttrib ["CondicionesDePago"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "SubTotal" )
			xAttribValue := hAttrib ["SubTotal"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "Moneda" )
			xAttribValue := hAttrib ["Moneda"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "Total" )
			xAttribValue := hAttrib ["Total"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "TipoDeComprobante" )
			xAttribValue := hAttrib ["TipoDeComprobante"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "MetodoPago" )
			xAttribValue := hAttrib ["MetodoPago"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "LugarExpedicion" )
			xAttribValue := hAttrib ["LugarExpedicion"] 
			
		ENDIF

	CASE cKey == "cfdi:Emisor" .AND. cPath == "cfdi:Comprobante"
		
		IF hb_HHasKey( hAttrib, "RegimenFiscal" )
			xAttribValue := hAttrib ["RegimenFiscal"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "Rfc" )
			xAttribValue := hAttrib ["Rfc"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "Nombre" )
			xAttribValue := hAttrib ["Nombre"] 
			
		ENDIF

	CASE cKey == "cfdi:Receptor" .AND. cPath == "cfdi:Comprobante"
		
		IF hb_HHasKey( hAttrib, "Rfc" )
			xAttribValue := hAttrib ["Rfc"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "Nombre" )
			xAttribValue := hAttrib ["Nombre"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "UsoCFDI" )
			xAttribValue := hAttrib ["UsoCFDI"] 
			
		ENDIF

	CASE cKey == "cfdi:Concepto" .AND. cPath == "cfdi:Comprobante/cfdi:Conceptos"
		
		IF hb_HHasKey( hAttrib, "ClaveProdServ" )
			xAttribValue := hAttrib ["ClaveProdServ"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "ClaveUnidad" )
			xAttribValue := hAttrib ["ClaveUnidad"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "Cantidad" )
			xAttribValue := hAttrib ["Cantidad"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "Unidad" )
			xAttribValue := hAttrib ["Unidad"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "NoIdentificacion" )
			xAttribValue := hAttrib ["NoIdentificacion"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "Descripcion" )
			xAttribValue := hAttrib ["Descripcion"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "ValorUnitario" )
			xAttribValue := hAttrib ["ValorUnitario"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "Importe" )
			xAttribValue := hAttrib ["Importe"] 
			
		ENDIF

	CASE cKey == "cfdi:Traslado" .AND. cPath == "cfdi:Comprobante/cfdi:Conceptos/cfdi:Concepto/cfdi:Impuestos/cfdi:Traslados"
		
		IF hb_HHasKey( hAttrib, "Base" )
			xAttribValue := hAttrib ["Base"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "Impuesto" )
			xAttribValue := hAttrib ["Impuesto"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "TipoFactor" )
			xAttribValue := hAttrib ["TipoFactor"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "TasaOCuota" )
			xAttribValue := hAttrib ["TasaOCuota"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "Importe" )
			xAttribValue := hAttrib ["Importe"] 
			
		ENDIF

	CASE cKey == "cfdi:Impuestos" .AND. cPath == "cfdi:Comprobante/cfdi:Conceptos/cfdi:Concepto"
		
	CASE cKey == "cfdi:Conceptos" .AND. cPath == "cfdi:Comprobante"
		
	CASE cKey == "cfdi:Impuestos" .AND. cPath == "cfdi:Comprobante"
		
		IF hb_HHasKey( hAttrib, "TotalImpuestosTrasladados" )
			xAttribValue := hAttrib ["TotalImpuestosTrasladados"] 
			
		ENDIF

	CASE cKey == "cfdi:Traslado" .AND. cPath == "cfdi:Comprobante/cfdi:Impuestos/cfdi:Traslados"
		
		IF hb_HHasKey( hAttrib, "Impuesto" )
			xAttribValue := hAttrib ["Impuesto"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "TipoFactor" )
			xAttribValue := hAttrib ["TipoFactor"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "TasaOCuota" )
			xAttribValue := hAttrib ["TasaOCuota"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "Importe" )
			xAttribValue := hAttrib ["Importe"] 
			
		ENDIF

	CASE cKey == "tfd:TimbreFiscalDigital" .AND. cPath == "cfdi:Comprobante/cfdi:Complemento"
		
		IF hb_HHasKey( hAttrib, "xmlns:tfd" )
			xAttribValue := hAttrib ["xmlns:tfd"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "xmlns:xsi" )
			xAttribValue := hAttrib ["xmlns:xsi"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "xsi:schemaLocation" )
			xAttribValue := hAttrib ["xsi:schemaLocation"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "Version" )
			xAttribValue := hAttrib ["Version"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "UUID" )
			xAttribValue := hAttrib ["UUID"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "FechaTimbrado" )
			xAttribValue := hAttrib ["FechaTimbrado"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "RfcProvCertif" )
			xAttribValue := hAttrib ["RfcProvCertif"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "SelloCFD" )
			xAttribValue := hAttrib ["SelloCFD"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "NoCertificadoSAT" )
			xAttribValue := hAttrib ["NoCertificadoSAT"] 
			
		ENDIF

		IF hb_HHasKey( hAttrib, "SelloSAT" )
			xAttribValue := hAttrib ["SelloSAT"] 
			
		ENDIF

END CASE
