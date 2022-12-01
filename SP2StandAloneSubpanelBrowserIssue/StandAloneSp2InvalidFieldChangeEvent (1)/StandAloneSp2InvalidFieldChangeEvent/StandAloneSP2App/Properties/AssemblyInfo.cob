       @OPTIONS RCS(ACP-UCS2),SRF(VAR),ALPHAL(WORD)
       @OPTIONS BINARY(WORD),PGMNAME(ALL),NOFLAGSW,SQLDCL
       IDENTIFICATION DIVISION.
       CLASS-ID. ASSEMBLY-INFO AS ".ASSEMBLY" CUSTOM-ATTRIBUTE IS 
           CA-ASSEMBLYTITLE CA-ASSEMBLYDESCRIPTION CA-ASSEMBLYCONFIGURATION
           CA-ASSEMBLYCOMPANY CA-ASSEMBLYPRODUCT CA-ASSEMBLYCOPYRIGHT
           CA-ASSEMBLYTRADEMARK CA-ASSEMBLYCULTURE
           CA-COMVISIBLE CA-GUID
           CA-ASSEMBLYVERSION CA-ASSEMBLYFILEVERSION.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           
           *> General Information about an assembly is controlled through the following
           *> set of attributes. Change these attribute values to modify the information
           *> associated with an assembly.
       SPECIAL-NAMES.
           CUSTOM-ATTRIBUTE CA-ASSEMBLYTITLE CLASS CLASS-ASSEMBLYTITLE
             USING N"StandAloneSP2App"
           CUSTOM-ATTRIBUTE CA-ASSEMBLYDESCRIPTION CLASS CLASS-ASSEMBLYDESCRIPTION
             USING PROP-EMPTYSTRING OF CLASS-CLRUTIL
           CUSTOM-ATTRIBUTE CA-ASSEMBLYCONFIGURATION CLASS CLASS-ASSEMBLYCONFIGURATION
             USING PROP-EMPTYSTRING OF CLASS-CLRUTIL
           CUSTOM-ATTRIBUTE CA-ASSEMBLYCOMPANY CLASS CLASS-ASSEMBLYCOMPANY
             USING PROP-EMPTYSTRING OF CLASS-CLRUTIL
           CUSTOM-ATTRIBUTE CA-ASSEMBLYPRODUCT CLASS CLASS-ASSEMBLYPRODUCT
             USING N"StandAloneSP2App"
           CUSTOM-ATTRIBUTE CA-ASSEMBLYCOPYRIGHT CLASS CLASS-ASSEMBLYCOPYRIGHT
             USING N"Copyright (C)  2020"
           CUSTOM-ATTRIBUTE CA-ASSEMBLYTRADEMARK CLASS CLASS-ASSEMBLYTRADEMARK
             USING PROP-EMPTYSTRING OF CLASS-CLRUTIL
           CUSTOM-ATTRIBUTE CA-ASSEMBLYCULTURE CLASS CLASS-ASSEMBLYCULTURE
             USING PROP-EMPTYSTRING OF CLASS-CLRUTIL
           
           *>  The following GUID is for the ID of the typelib if this project is exposed to COM
           CUSTOM-ATTRIBUTE CA-GUID CLASS CLASS-GUID
             USING N"c01e108b-1763-409d-8c16-345a7fc6317e"
           
           *> Setting ComVisible to B"0" makes the types in this assembly not visible 
           *> to COM components.  If you need to access a type in this assembly from 
           *> COM, set the ComVisible attribute to B"1" on that type.
           CUSTOM-ATTRIBUTE CA-COMVISIBLE CLASS CLASS-COMVISIBLE
             USING B"0"
           
           *> Version information for an assembly consists of the following four values:
           *>
           *>      Major Version
           *>      Minor Version
           *>      Build Number
           *>      Revision
           *>
           CUSTOM-ATTRIBUTE CA-ASSEMBLYVERSION CLASS CLASS-ASSEMBLYVERSION
             USING N"1.0.0.0"
           CUSTOM-ATTRIBUTE CA-ASSEMBLYFILEVERSION CLASS CLASS-ASSEMBLYFILEVERSION
             USING N"1.0.0.0"
             .
       REPOSITORY.
           CLASS CLASS-CLRUTIL AS "Fujitsu.COBOL.CLRUtil"
           CLASS CLASS-ASSEMBLYTITLE AS "System.Reflection.AssemblyTitleAttribute"
           CLASS CLASS-ASSEMBLYDESCRIPTION AS "System.Reflection.AssemblyDescriptionAttribute"
           CLASS CLASS-ASSEMBLYCONFIGURATION AS "System.Reflection.AssemblyConfigurationAttribute"
           CLASS CLASS-ASSEMBLYCOMPANY AS "System.Reflection.AssemblyCompanyAttribute"
           CLASS CLASS-ASSEMBLYPRODUCT AS "System.Reflection.AssemblyProductAttribute"
           CLASS CLASS-ASSEMBLYCOPYRIGHT AS "System.Reflection.AssemblyCopyrightAttribute"
           CLASS CLASS-ASSEMBLYTRADEMARK AS "System.Reflection.AssemblyTrademarkAttribute"
           CLASS CLASS-ASSEMBLYCULTURE AS "System.Reflection.AssemblyCultureAttribute"
           CLASS CLASS-GUID AS "System.Runtime.InteropServices.GuidAttribute"
           CLASS CLASS-COMVISIBLE AS "System.Runtime.InteropServices.ComVisibleAttribute"
           CLASS CLASS-ASSEMBLYVERSION AS "System.Reflection.AssemblyVersionAttribute"
           CLASS CLASS-ASSEMBLYFILEVERSION AS "System.Reflection.AssemblyFileVersionAttribute"
           PROPERTY PROP-EMPTYSTRING AS "EmptyString"
           .
       END CLASS ASSEMBLY-INFO.
