/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Program protection
 * Demo was contributed to HMG forum by Edward 20/Nov/2023
 */

#include <hmg.ch>

#pragma TEXTHIDDEN(1)       /* obfuscating strings in an executable file */

/*
 * FUNCTION Main ( xMode )
 *
 * This is the main entry point of the application. It retrieves PC properties,
 * generates a unique PC ID, and checks if the application is activated on the
 * current computer.
 *
 * Parameters:
 *   xMode : An optional string parameter. If equal to "/ActivationByLicensor",
 *           it activates the application by writing the PC ID to an INI file.
 *           If omitted, the application checks for existing activation.
 *
 * Returns:
 *   None. This function does not return a value. It either continues the
 *   application execution or terminates it if the activation check fails.
 *
 * Purpose:
 *   This function implements a simple software protection mechanism. It aims to
 *   prevent unauthorized use of the application by tying it to a specific
 *   computer. The function retrieves hardware and software properties of the
 *   computer, generates a unique ID based on these properties, and stores this
 *   ID in an INI file. Upon subsequent runs, the function compares the current
 *   PC ID with the stored ID. If they match, the application proceeds; otherwise,
 *   it displays an error message and terminates.
 *
 * Notes:
 *   - The INI file "example.ini" is created in the application's current working
 *     directory.
 *   - The PC ID is generated using the SHA384 hashing algorithm.
 *   - The function uses the GetPCProperties() function to retrieve PC properties.
 *   - The effectiveness of this protection mechanism depends on the properties
 *     selected by GetPCProperties() and the robustness of the SHA384 hashing algorithm.
 *   - This is a basic protection scheme and can be bypassed by determined users.
 */
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

/*
 * FUNCTION WMIService()
 *
 * Creates and returns a WMI (Windows Management Instrumentation) object for
 * accessing system information.
 *
 * Parameters:
 *   None.
 *
 * Returns:
 *   A WMI object connected to the "root\cimv2" namespace. This object can be
 *   used to query various system properties and settings.
 *
 * Purpose:
 *   This function simplifies the process of accessing WMI data by encapsulating
 *   the object creation and connection steps. It provides a reusable way to
 *   obtain a WMI object for querying system information, such as CPU, disk,
 *   and network adapter details.
 *
 * Notes:
 *   - This function relies on the win_oleCreateObject function to create the
 *     WMI object.
 *   - The function connects to the "root\cimv2" namespace, which is the standard
 *     namespace for accessing system information.
 */
FUNCTION WMIService()

   LOCAL oWMI, oLocator

   oLocator := win_oleCreateObject( "wbemScripting.SwbemLocator" )
   oWMI := oLocator:ConnectServer( , "root\cimv2" )

RETURN oWMI

/*
 * FUNCTION GetPCProperties ( lAll )
 *
 * Retrieves various properties of the PC, including CPU, disk drive, and
 * network adapter information, using WMI (Windows Management Instrumentation).
 *
 * Parameters:
 *   lAll : A logical value indicating whether to retrieve all available
 *          properties or only a subset of essential properties.
 *          .T. retrieves all properties.
 *          .F. (default) retrieves a limited set of properties.
 *
 * Returns:
 *   A string containing the collected PC properties, formatted as
 *   "Property: Value" pairs, separated by carriage return and line feed
 *   characters (CRLF).
 *
 * Purpose:
 *   This function is designed to gather information about the computer's
 *   hardware and software configuration. The collected properties can be used
 *   for various purposes, such as generating a unique PC ID for software
 *   licensing, system inventory, or troubleshooting. The lAll parameter
 *   allows controlling the level of detail retrieved, balancing the need for
 *   comprehensive information with performance considerations.
 *
 * Notes:
 *   - This function uses the WMIService() function to obtain a WMI object.
 *   - The function queries the Win32_Processor, Win32_DiskDrive,
 *     Win32_NetworkAdapter, Win32_NetworkAdapterConfiguration, and
 *     Win32_ComputerSystemProduct WMI classes.
 *   - The function uses hb_ValToExp() to convert property values to strings.
 *   - The function filters the Win32_NetworkAdapter query to retrieve only
 *     physical adapters (PhysicalAdapter = TRUE).
 *   - The function filters the Win32_NetworkAdapterConfiguration query to
 *     retrieve only configurations where IP is enabled (IPEnabled = TRUE).
 */
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
