/**********************************************************************************
*
*  Script name : weblog.sas
*  Description : Exploration on browsing log data
*  Author      : Nur Andi
*  Create date : 29 Oct 2014
*  SAS version : 9.2
* 
***********************************************************************************/


/***********************************************************************************
*  Stage1 :
*  Read data from external file (delimited text)
************************************************************************************/

data weblog ;
infile 'C:\Users\NANDI\Documents\Documents\NANDI\cfs-syslog-2014-10-00-00-53.1413824504337.log'
delimiter = '09'x MISSOVER DSD lrecl=32767 ;
	informat sec_id $40. ;
	informat ip $15. ;
	informat acc_date yymmdd10. ;
	informat url $279. ;
	informat agent $230. ;
	informat log_long $718. ;
	informat scr_res $10. ;
	informat scr_cat $6. ;
	informat alt_agent $240. ;
	
	format sec_id $40. ;
	format ip $15. ;
	format acc_date yymmdd10. ;
	format url $279. ;
	format agent $230. ;
	format log_long $718. ;
	format scr_res $10. ;
	format scr_cat $6. ;
	format alt_agent $240. ;
input
	sec_id $ ip $ acc_date url $ agent $ log_long $ scr_res $ scr_cat $ alt_agent $ ; 
drop alt_agent;
run;



/***********************************************************************************
*  Stage2 :
*  Filter bad data
************************************************************************************/

data weblog;
	set weblog;
	where acc_date is not null;
run;



/***********************************************************************************
*  Stage3 :
*  Parse/extract information from unstructured fields, e.g
*  . Domain name
*  . Operating system (OS)
*  . Customer ID
*  . Date and time
*  . Location
*  . Package name
************************************************************************************/

data weblog_str;
set weblog;

/** Extracting domain name from URL **/
domain = prxchange('s/http:\/\/|www[1-9]?\.//', -1, url);
slash_idx = index(domain,'/');
if slash_idx ^= 0 then do; 
	domain = substr(domain,1,slash_idx-1); 
end;

/** Extracting OS **/
os_pattern = '/(windows|symbian|android|ubuntu|linux|ios|blackberry|bb10)([^;\)\/]+)/i';
os_pattern_prx = prxparse(os_pattern);
		call prxsubstr(os_pattern_prx, agent, p1, l1);
	if p1 ^= 0 then do; os = substr(agent,p1,l1); end;

/** Extracting Cust ID **/
id_pattern = '/[0-9]{12}/';
id_pattern_prx = prxparse(id_pattern);
call prxsubstr(id_pattern_prx, log_long, p1, l1);
if p1 ^= 0 then do; cust_id = substr(log_long,p1,l1); end;

/** Extracting Date & Time **/
tm_pattern = '/[0-9]{4}\-[0-9]{2}\-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}/';
tm_pattern_prx = prxparse(tm_pattern);
call prxsubstr(tm_pattern_prx, log_long, p1, l1);
if p1 ^= 0 then do; transc_ts = substr(log_long,p1,l1); end;

transc_dt = input(scan(transc_ts,1,' '),??yymmdd10.) ;
transc_tm = input(scan(transc_ts,2,' '),??time8.) ;
format transc_dt yymmdd10. transc_tm time8.;

/** Extracting Geo-Location **/
gl_pattern = '/bras[a-z0-9]*\-[a-z0-9]+\-[a-z0-9]+[^"]/i';
gl_pattern_prx = prxparse(gl_pattern);
call prxsubstr(gl_pattern_prx, log_long, p1, l1);
if p1 ^= 0 then do; location = substr(log_long,p1,l1); end;

/** Extracting Package **/
pg_pattern = '/INET[^"]+/';
pg_pattern_prx = prxparse(pg_pattern);
call prxsubstr(pg_pattern_prx, log_long, p1, l1);
if p1 ^= 0 then do; package = substr(log_long,p1,l1); end;

unq_id = _N_;

keep sec_id ip cust_id transc_dt transc_tm 
	url domain os scr_res scr_cat location package unq_id; 

run;



/***********************************************************************************
*  Stage4 :
*  Parse/extract search keyword from several search engine domain, eg.
*  . Google
*  . Yahoo
*  . Kaskus
*  . Bing
*  . Lazada
*  . Berniaga
*  . Youtube
*  . OLX
************************************************************************************/

data weblog_search;
set weblog_str;
length pattern $ 30;
keep unq_id keyword;

if prxmatch('/(google|kaskus|lazada|bing|berniaga).+q=/',url) ^= 0
then
	pattern = '/q=([^\\&]+)/i';
else if prxmatch('/yahoo.+p=/',url) ^= 0
then 
	pattern = '/p=([^\\&]+)/i';
else if prxmatch('/olx.+mencari/',url) ^= 0
then
	pattern = '/mencari\/([^\/]+)/';
else if prxmatch('/youtube.+search_query=/',url) ^= 0
then 
	pattern = '/_query=([^\\&]+)/i';

	search_pattern = prxparse(pattern);
		call prxsubstr(search_pattern, url, p1, l1);
	if p1 ^= 0 then do; 
		keyword = strip(prxchange('s/%..|\+| +|q=|p=|mencari\/|_query=|\|/ /', -1, substr(url,p1,l1))); 
		end;

where prxmatch('/(google|kaskus|lazada|bing|berniaga).+q=/',url) ^= 0 
	or prxmatch('/yahoo.+p=/',url) ^= 0
	or prxmatch('/olx.+mencari/',url) ^= 0
	or prxmatch('/youtube.+search_query=/',url) ^= 0;
run;



/***********************************************************************************
*  Stage5 : Final
*  Merge data sets
************************************************************************************/

data weblog_final;
merge weblog_str weblog_search ;
by unq_id;
	if missing(keyword) then is_search = 0;
	else is_search = 1;
drop unq_id;
run;

