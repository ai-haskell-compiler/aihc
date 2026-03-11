Name:           B
Version:        1
Release:        1%{?dist}
Summary:        package B
License:        GPL
BuildRequires:  C

%description
Test package B

%package devel
Summary: B development

%description devel
B development files.

%prep

%build

%install

%files

%files devel

%changelog
* Wed Jul 29 13:38:27 +08 2020 Jens Petersen <petersen@redhat.com>
-
