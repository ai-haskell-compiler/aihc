/*-
 * Copyright (c) 2006 Aaron Griffin <aaronmgriffin@gmail.com>
 *
 * Based on original libfetch code from:
 * Copyright (c) 1998-2004 Dag-Erling Coïdan Smørgrav
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer
 *    in this position and unchanged.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <sys/cdefs.h>

#include <sys/param.h>
#include <sys/errno.h>

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "download.h"
#include "common.h"

auth_t downloadAuthMethod;
int downloadLastErrCode;
char downloadLastErrString[MAXERRSTRING];
int downloadTimeout;
int downloadRestartCalls = 1;
int downloadDebug;


/*** Local data **************************************************************/

/*
 * Error messages for parser errors
 */
#define URL_MALFORMED		1
#define URL_BAD_SCHEME		2
#define URL_BAD_PORT		3
static struct downloaderr _url_errlist[] = {
	{ URL_MALFORMED,	DLERR_URL,	"Malformed URL" },
	{ URL_BAD_SCHEME,	DLERR_URL,	"Invalid URL scheme" },
	{ URL_BAD_PORT,		DLERR_URL,	"Invalid server port" },
	{ -1,			DLERR_UNKNOWN,	"Unknown parser error" }
};


/*** Public API **************************************************************/

/*
 * Select the appropriate protocol for the URL scheme, and return a
 * read-only stream connected to the document referenced by the URL.
 * Also fill out the struct url_stat.
 */
FILE *
downloadXGet(struct url *URL, struct url_stat *us, const char *flags)
{
	int direct;

	direct = CHECK_FLAG('d');
	if (us != NULL) {
		us->size = -1;
		us->atime = us->mtime = 0;
	}
	if (strcasecmp(URL->scheme, SCHEME_FILE) == 0)
		return (downloadXGetFile(URL, us, flags));
	else if (strcasecmp(URL->scheme, SCHEME_FTP) == 0)
		return (downloadXGetFTP(URL, us, flags));
	else if (strcasecmp(URL->scheme, SCHEME_HTTP) == 0)
		return (downloadXGetHTTP(URL, us, flags));
	else if (strcasecmp(URL->scheme, SCHEME_HTTPS) == 0)
		return (downloadXGetHTTP(URL, us, flags));
	_url_seterr(URL_BAD_SCHEME);
	return (NULL);
}

/*
 * Select the appropriate protocol for the URL scheme, and return a
 * read-only stream connected to the document referenced by the URL.
 */
FILE *
downloadGet(struct url *URL, const char *flags)
{
	return (downloadXGet(URL, NULL, flags));
}

/*
 * Select the appropriate protocol for the URL scheme, and return the
 * size of the document referenced by the URL if it exists.
 */
int
downloadStat(struct url *URL, struct url_stat *us, const char *flags)
{
	int direct;

	direct = CHECK_FLAG('d');
	if (us != NULL) {
		us->size = -1;
		us->atime = us->mtime = 0;
	}
	if (strcasecmp(URL->scheme, SCHEME_FILE) == 0)
		return (downloadStatFile(URL, us, flags));
	else if (strcasecmp(URL->scheme, SCHEME_FTP) == 0)
		return (downloadStatFTP(URL, us, flags));
	else if (strcasecmp(URL->scheme, SCHEME_HTTP) == 0)
		return (downloadStatHTTP(URL, us, flags));
	else if (strcasecmp(URL->scheme, SCHEME_HTTPS) == 0)
		return (downloadStatHTTP(URL, us, flags));
	_url_seterr(URL_BAD_SCHEME);
	return (-1);
}

/*
 * Attempt to parse the given URL; if successful, call downloadXGet().
 */
FILE *
downloadXGetURL(const char *URL, struct url_stat *us, const char *flags)
{
	struct url *u;
	FILE *f;

	if ((u = downloadParseURL(URL)) == NULL)
		return (NULL);

	f = downloadXGet(u, us, flags);

	downloadFreeURL(u);
	return (f);
}

/*
 * Attempt to parse the given URL; if successful, call downloadGet().
 */
FILE *
downloadGetURL(const char *URL, const char *flags)
{
	return (downloadXGetURL(URL, NULL, flags));
}

/*
 * Attempt to parse the given URL; if successful, call downloadStat().
 */
int
downloadStatURL(const char *URL, struct url_stat *us, const char *flags)
{
	struct url *u;
	int s;

	if ((u = downloadParseURL(URL)) == NULL)
		return (-1);

	s = downloadStat(u, us, flags);

	downloadFreeURL(u);
	return (s);
}

/*
 * Make a URL
 */
struct url *
downloadMakeURL(const char *scheme, const char *host, int port, const char *doc,
    const char *user, const char *pwd)
{
	struct url *u;

	if (!scheme || (!host && !doc)) {
		_url_seterr(URL_MALFORMED);
		return (NULL);
	}

	if (port < 0 || port > 65535) {
		_url_seterr(URL_BAD_PORT);
		return (NULL);
	}

	/* allocate struct url */
	if ((u = calloc(1, sizeof(*u))) == NULL) {
		_download_syserr();
		return (NULL);
	}

	if ((u->doc = strdup(doc ? doc : "/")) == NULL) {
		_download_syserr();
		free(u);
		return (NULL);
	}

#define seturl(x) snprintf(u->x, sizeof(u->x), "%s", x)
	seturl(scheme);
	seturl(host);
	seturl(user);
	seturl(pwd);
#undef seturl
	u->port = port;

	return (u);
}

/*
 * Split an URL into components. URL syntax is:
 * [method:/][/[user[:pwd]@]host[:port]/][document]
 * This almost, but not quite, RFC1738 URL syntax.
 */
struct url *
downloadParseURL(const char *URL)
{
	char *doc;
	const char *p, *q;
	struct url *u;
	int i;

	/* allocate struct url */
	if ((u = calloc(1, sizeof(*u))) == NULL) {
		_download_syserr();
		return (NULL);
	}

	/* scheme name */
	if ((p = strstr(URL, ":/"))) {
		snprintf(u->scheme, URL_SCHEMELEN+1,
		    "%.*s", (int)(p - URL), URL);
		URL = ++p;
		/*
		 * Only one slash: no host, leave slash as part of document
		 * Two slashes: host follows, strip slashes
		 */
		if (URL[1] == '/')
			URL = (p += 2);
	} else {
		p = URL;
	}
	if (!*URL || *URL == '/' || *URL == '.' ||
	    (u->scheme[0] == '\0' &&
		strchr(URL, '/') == NULL && strchr(URL, ':') == NULL))
		goto nohost;

	p = strpbrk(URL, "/@");
	if (p && *p == '@') {
		/* username */
		for (q = URL, i = 0; (*q != ':') && (*q != '@'); q++)
			if (i < URL_USERLEN)
				u->user[i++] = *q;

		/* password */
		if (*q == ':')
			for (q++, i = 0; (*q != ':') && (*q != '@'); q++)
				if (i < URL_PWDLEN)
					u->pwd[i++] = *q;

		p++;
	} else {
		p = URL;
	}

	/* hostname */
#ifdef INET6
	if (*p == '[' && (q = strchr(p + 1, ']')) != NULL &&
	    (*++q == '\0' || *q == '/' || *q == ':')) {
		if ((i = q - p - 2) > MAXHOSTNAMELEN)
			i = MAXHOSTNAMELEN;
		strncpy(u->host, ++p, i);
		p = q;
	} else
#endif
		for (i = 0; *p && (*p != '/') && (*p != ':'); p++)
			if (i < MAXHOSTNAMELEN)
				u->host[i++] = *p;

	/* port */
	if (*p == ':') {
		for (q = ++p; *q && (*q != '/'); q++)
			if (isdigit(*q))
				u->port = u->port * 10 + (*q - '0');
			else {
				/* invalid port */
				_url_seterr(URL_BAD_PORT);
				goto ouch;
			}
		p = q;
	}

nohost:
	/* document */
	if (!*p)
		p = "/";

	if (strcasecmp(u->scheme, SCHEME_HTTP) == 0 ||
	    strcasecmp(u->scheme, SCHEME_HTTPS) == 0) {
		const char hexnums[] = "0123456789abcdef";

		/* percent-escape whitespace. */
		if ((doc = malloc(strlen(p) * 3 + 1)) == NULL) {
			_download_syserr();
			goto ouch;
		}
		u->doc = doc;
		while (*p != '\0') {
			if (!isspace(*p)) {
				*doc++ = *p++;
			} else {
				*doc++ = '%';
				*doc++ = hexnums[((unsigned int)*p) >> 4];
				*doc++ = hexnums[((unsigned int)*p) & 0xf];
				p++;
			}
		}
		*doc = '\0';
	} else if ((u->doc = strdup(p)) == NULL) {
		_download_syserr();
		goto ouch;
	}

	DBG(fprintf(stderr,
		  "scheme:   [%s]\n"
		  "user:     [%s]\n"
		  "password: [%s]\n"
		  "host:     [%s]\n"
		  "port:     [%d]\n"
		  "document: [%s]\n",
		  u->scheme, u->user, u->pwd,
		  u->host, u->port, u->doc));

	return (u);

ouch:
	free(u);
	return (NULL);
}

/*
 * Free a URL
 */
void
downloadFreeURL(struct url *u)
{
	free(u->doc);
	free(u);
}
