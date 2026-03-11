Name:           B
Version:        1
Release:        1%{?dist}
Summary:        package B
License:        GPL

%description
Test package B

%prep

%build

%install

%files
%{_libdir}/cmake/libB
%{_libdir}/cmake/libB/B.cmake

%changelog
* Wed Jul 29 13:38:27 +08 2020 Jens Petersen <petersen@redhat.com>
-
