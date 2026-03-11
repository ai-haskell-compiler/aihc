%bcond_with boot

Name:           C
Version:        1
Release:        1%{?dist}
Summary:        package C
License:        GPL
%if %{without boot}
BuildRequires:  C-A
%endif

%description
Circular dep

%files

%changelog
* Wed Jul 29 13:38:27 +08 2020 Jens Petersen <petersen@redhat.com>
-
