#define MyAppName "Unison"
#define MyAppURL "http://www.cis.upenn.edu/~bcpierce/unison/"
#define MyAppExeName "unison 2.48.4 GTK.exe"
#define ProductVersion "2.48.4"

[Setup]
AppId={{2B41C63F-C8D0-411C-8BB5-8AACD2BA33ED}
AppName={#MyAppName}
AppVersion={#ProductVersion}
AppVerName={#MyAppName} {#ProductVersion}
AppCopyright=Benjamin Pierce
AppPublisher=Benjamin Pierce
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}/lists.html
CreateAppDir=yes
DefaultDirName={pf}\{#MyAppName}
DefaultGroupName={#MyAppName}
DisableDirPage=auto
DisableProgramGroupPage=yes
LicenseFile=../LICENSE
InfoAfterFile=postinstall.txt
OutputDir=.
OutputBaseFilename=unison{#ProductVersion}_setup
Compression=lzma
SolidCompression=yes
UninstallDisplayIcon={app}\{#MyAppExeName}
VersionInfoVersion={#ProductVersion}

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "french"; MessagesFile: "compiler:Languages\French.isl"
Name: "german"; MessagesFile: "compiler:Languages\German.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"

[Files]
Source: "plink.bat"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\bin\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#MyAppName}}"; Flags: postinstall shellexec

[Messages]
BeveledLabel=Inno Setup
