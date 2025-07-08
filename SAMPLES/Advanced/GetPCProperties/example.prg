/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Program protection
 * Demo was contributed to HMG forum by Edward 20/Nov/2023
 */

#include <hmg.ch>

#pragma TEXTHIDDEN(1)       /* obfuscating strings in an executable file */

FUNCTION Main ( xMode )

   LOCAL cPC_Properties := GetPCProperties ( .F. )
   LOCAL cPC_ID := hb_SHA384 ( cPC_Properties )
   LOCAL cINIFile := hb_cwd() + "example.ini"
   LOCAL hINI

   IF ! File( cINIFile )
      hINI := hb_iniNew( .F. )
      hb_HSet ( hINI, "ID", "" )
      hb_iniWrite( cINIFile, hINI )
   ENDIF

   hb_iniSetComment( Chr ( 30 ), Chr ( 31 ) )

   hINI := hb_iniRead( cINIFile, , , .F. )

   IF .NOT. Empty ( xMode ) .AND. xMode == "/ActivationByLicensor"
      hINI[ "ID" ] := cPC_ID
      hb_iniWrite( cINIFile, hINI )
   ENDIF

   IF .NOT. ( cPC_ID == hINI[ "ID" ] )
      MsgStop ( "This computer does not have the correct activation code for the application to work.", "STOP!" )
      CLOSE DATA
      QUIT
   ENDIF

   MsgInfo ( "Your PC Properties :" + CRLF + ;
      cPC_Properties + CRLF + CRLF + ;
      "Your PC ID encrypted hash code :" + CRLF + ;
      cPC_ID )

RETURN

// ***********************************
FUNCTION WMIService()

   LOCAL oWMI, oLocator

   oLocator := win_oleCreateObject( "wbemScripting.SwbemLocator" )
   oWMI := oLocator:ConnectServer( , "root\cimv2" )

RETURN oWMI
// ***********************************
FUNCTION GetPCProperties ( lAll )

   LOCAL oWMI, objItem
   LOCAL aProperties
   LOCAL cData := "", cProperty

   DEFAULT lAll := .F.

   oWMI := WMIService()

   // CPUs
   IF lAll
      aProperties := { "AddressWidth", "Architecture", "AssetTag", "Availability", "Caption", "Characteristics", ;
         "ConfigManagerErrorCode", "ConfigManagerUserConfig", "CpuStatus", "CreationClassName", ;
         "CurrentClockSpeed", "CurrentVoltage", "DataWidth", "Description", "DeviceID", "ErrorCleared", ;
         "ErrorDescription", "ExtClock", "Family", "InstallDate", "L2CacheSize", "L2CacheSpeed", ;
         "L3CacheSize", "L3CacheSpeed", "LastErrorCode", "Level", "LoadPercentage", "Manufacturer", ;
         "MaxClockSpeed", "Name", "NumberOfCores", "NumberOfEnabledCore", "NumberOfLogicalProcessors", ;
         "OtherFamilyDescription", "PartNumber", "PNPDeviceID", "PowerManagementCapabilities", ;
         "PowerManagementSupported", "ProcessorId", "ProcessorType", "Revision", "Role", ;
         "SecondLevelAddressTranslationExtensions", "SerialNumber", "SocketDesignation", "Status", ;
         "StatusInfo", "Stepping", "SystemCreationClassName", "SystemName", "ThreadCount", "UniqueId", ;
         "UpgradeMethod", "Version", "VirtualizationFirmwareEnabled", "VMMonitorModeExtensions", ;
         "VoltageCaps" }
   ELSE
      aProperties := { "Name", "ProcessorId" }
   ENDIF

   FOR EACH objItem IN oWMI:ExecQuery( "Select * from Win32_Processor" )
      cData += "CPU: " + CRLF
      FOR EACH cProperty IN aProperties
         cData += cProperty + ": " + hb_ValToExp ( objItem:&( cProperty ) ) + CRLF
      NEXT
   NEXT

   // Disk Drivers
   IF lAll
      aProperties := { "Availability", "BytesPerSector", "Capabilities", "CapabilityDescriptions", ;
         "Caption", "CompressionMethod", "ConfigManagerErrorCode", "ConfigManagerUserConfig", ;
         "CreationClassName", "DefaultBlockSize", "Description", "DeviceID", "ErrorCleared", ;
         "ErrorDescription", "ErrorMethodology", "FirmwareRevision", "Index", "InstallDate", ;
         "InterfaceType", "LastErrorCode", "Manufacturer", "MaxBlockSize", "MaxMediaSize", ;
         "MediaLoaded", "MediaType", "MinBlockSize", "Model", "Name", "NeedsCleaning", ;
         "NumberOfMediaSupported", "Partitions", "PNPDeviceID", "PowerManagementCapabilities", ;
         "PowerManagementSupported", "SCSIBus", "SCSILogicalUnit", "SCSIPort", "SCSITargetId", ;
         "SectorsPerTrack", "SerialNumber", "Signature", "Size", "Status", "StatusInfo", ;
         "SystemCreationClassName", "SystemName", "TotalCylinders", "TotalHeads", "TotalSectors", ;
         "TotalTracks", "TracksPerCylinder" }

   ELSE
      aProperties := { "FirmwareRevision", "SerialNumber", "Signature" }
   ENDIF

   FOR EACH objItem IN oWMI:ExecQuery( "Select * from Win32_DiskDrive" )
      cData += "DISK: " + CRLF
      FOR EACH cProperty IN aProperties
         cData += cProperty + ": " + hb_ValToExp ( objItem:&( cProperty ) ) + CRLF
      NEXT
   NEXT

   // NICs
   IF lAll
      aProperties := { "AdapterType", "AdapterTypeId", "AutoSense", "Availability", "Caption", ;
         "ConfigManagerErrorCode", "ConfigManagerUserConfig", "CreationClassName", ;
         "Description", "DeviceID", "ErrorCleared", "ErrorDescription", "GUID", ;
         "Index", "InstallDate", "Installed", "InterfaceIndex", "LastErrorCode", ;
         "MACAddress", "Manufacturer", "MaxNumberControlled", "MaxSpeed", "Name", ;
         "NetConnectionID", "NetConnectionStatus", "NetEnabled", "NetworkAddresses", ;
         "PermanentAddress", "PhysicalAdapter", "PNPDeviceID", "PowerManagementCapabilities", ;
         "PowerManagementSupported", "ProductName", "ServiceName", "Speed", "Status", ;
         "StatusInfo", "SystemCreationClassName", "SystemName", "TimeOfLastReset" }
   ELSE
      aProperties := { "GUID", "MACAddress" }
   ENDIF

   FOR EACH objItem IN oWMI:ExecQuery( "Select * from Win32_NetworkAdapter Where PhysicalAdapter = TRUE" )
      cData += "NIC: " + CRLF
      FOR EACH cProperty IN aProperties
         cData += cProperty + ": " + hb_ValToExp ( objItem:&( cProperty ) ) + CRLF
      NEXT
   NEXT

   // NIC Configs
   IF lAll
      aProperties := { "ArpAlwaysSourceRoute", "ArpUseEtherSNAP", "Caption", "DatabasePath", ;
         "DeadGWDetectEnabled", "DefaultIPGateway", "DefaultTOS", "DefaultTTL", ;
         "Description", "DHCPEnabled", "DHCPLeaseExpires", "DHCPLeaseObtained", ;
         "DHCPServer", "DNSDomain", "DNSDomainSuffixSearchOrder", "DNSEnabledForWINSResolution", ;
         "DNSHostName", "DNSServerSearchOrder", "DomainDNSRegistrationEnabled", ;
         "ForwardBufferMemory", "FullDNSRegistrationEnabled", "GatewayCostMetric", ;
         "IGMPLevel", "Index", "InterfaceIndex", "IPAddress", "IPConnectionMetric", ;
         "IPEnabled", "IPFilterSecurityEnabled", "IPPortSecurityEnabled", "IPSecPermitIPProtocols", ;
         "IPSecPermitTCPPorts", "IPSecPermitUDPPorts", "IPSubnet", "IPUseZeroBroadcast", ;
         "IPXAddress", "IPXEnabled", "IPXFrameType", "IPXMediaType", "IPXNetworkNumber", ;
         "IPXVirtualNetNumber", "KeepAliveInterval", "KeepAliveTime", "MACAddress", ;
         "MTU", "NumForwardPackets", "PMTUBHDetectEnabled", "PMTUDiscoveryEnabled", ;
         "ServiceName", "SettingID", "TcpipNetbiosOptions", "TcpMaxConnectRetransmissions", ;
         "TcpMaxDataRetransmissions", "TcpNumConnections", "TcpUseRFC1122UrgentPointer", ;
         "TcpWindowSize", "WINSEnableLMHostsLookup", "WINSHostLookupFile", "WINSPrimaryServer", ;
         "WINSScopeID", "WINSSecondaryServer" }

   ELSE
      aProperties := {}
   ENDIF

   FOR EACH objItem IN oWMI:ExecQuery( "Select * From Win32_NetworkAdapterConfiguration Where IPEnabled = TRUE" )
      FOR EACH cProperty IN aProperties
         cData += cProperty + ": " + hb_ValToExp ( objItem:&( cProperty ) ) + CRLF
      NEXT
   NEXT

   // Computer System Product
   IF lAll
      aProperties := { "Caption", "Description", "IdentifyingNumber", "Name", "SKUNumber", ;
         "Vendor", "Version", "UUID" }
   ELSE
      aProperties := { "IdentifyingNumber", "Name", "SKUNumber", "Vendor", "UUID" }
   ENDIF

   FOR EACH objItem IN oWMI:ExecQuery( "Select * From Win32_ComputerSystemProduct" )
      cData += "CSP: " + CRLF
      FOR EACH cProperty IN aProperties
         cData += cProperty + ": " + hb_ValToExp ( objItem:&( cProperty ) ) + CRLF
      NEXT
   NEXT

RETURN cData

#pragma TEXTHIDDEN(0)
