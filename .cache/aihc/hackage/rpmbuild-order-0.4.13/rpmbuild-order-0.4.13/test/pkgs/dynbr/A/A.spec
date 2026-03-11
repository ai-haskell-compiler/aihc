Name:           A
Version:        1
Release:        1%{?dist}
Summary:        package A
License:        GPL

%description
Test package A

%generate_buildrequires
echo B-devel

%package -n C-A
Summary: A for C

%description -n C-A
A support for C

%files

%files -n C-A

%changelog
* Wed Jul 29 13:38:27 +08 2020 Jens Petersen <petersen@redhat.com>
-
