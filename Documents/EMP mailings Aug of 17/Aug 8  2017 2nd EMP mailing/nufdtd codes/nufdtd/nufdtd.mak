# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=nufdtd - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to nufdtd - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "nufdtd - Win32 Release" && "$(CFG)" != "nufdtd - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "nufdtd.mak" CFG="nufdtd - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "nufdtd - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "nufdtd - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "nufdtd - Win32 Debug"
F90=fl32.exe
RSC=rc.exe

!IF  "$(CFG)" == "nufdtd - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=.\Release
INTDIR=.\Release

ALL : "$(OUTDIR)\nufdtd.exe"

CLEAN : 
	-@erase ".\Release\nufdtd.exe"
	-@erase ".\Release\nufdtd3d.obj"
	-@erase ".\Release\nufdtd.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Ox /I "Release/" /c /nologo
# ADD F90 /Ox /I "Release/" /c /nologo
F90_PROJ=/Ox /I "Release/" /c /nologo /Fo"Release/" 
F90_OBJS=.\Release/
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/nufdtd.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/nufdtd.pdb" /machine:I386 /out:"$(OUTDIR)/nufdtd.exe" 
LINK32_OBJS= \
	"$(INTDIR)/nufdtd3d.obj" \
	"$(INTDIR)/nufdtd.obj"

"$(OUTDIR)\nufdtd.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "nufdtd - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "$(OUTDIR)\nufdtd.exe"

CLEAN : 
	-@erase ".\Debug\nufdtd.exe"
	-@erase ".\Debug\nufdtd3d.obj"
	-@erase ".\Debug\nufdtd.obj"
	-@erase ".\Debug\nufdtd.ilk"
	-@erase ".\Debug\nufdtd.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Zi /I "Debug/" /c /nologo
# ADD F90 /Zi /I "Debug/" /c /nologo
F90_PROJ=/Zi /I "Debug/" /c /nologo /Fo"Debug/" /Fd"Debug/nufdtd.pdb" 
F90_OBJS=.\Debug/
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/nufdtd.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/nufdtd.pdb" /debug /machine:I386 /out:"$(OUTDIR)/nufdtd.exe" 
LINK32_OBJS= \
	"$(INTDIR)/nufdtd3d.obj" \
	"$(INTDIR)/nufdtd.obj"

"$(OUTDIR)\nufdtd.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.for{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f90{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

################################################################################
# Begin Target

# Name "nufdtd - Win32 Release"
# Name "nufdtd - Win32 Debug"

!IF  "$(CFG)" == "nufdtd - Win32 Release"

!ELSEIF  "$(CFG)" == "nufdtd - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\nufdtd3d.f
DEP_F90_NUFDT=\
	".\nufdtd3d_params.f"\
	

"$(INTDIR)\nufdtd3d.obj" : $(SOURCE) $(DEP_F90_NUFDT) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\nufdtd.f
DEP_F90_NUFDTD=\
	".\nufdtd_params.f"\
	

"$(INTDIR)\nufdtd.obj" : $(SOURCE) $(DEP_F90_NUFDTD) "$(INTDIR)"


# End Source File
# End Target
# End Project
################################################################################
