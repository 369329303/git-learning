# TBSG 4.0 ACL 模块实现分析
## 0.
```
SSLExtendConfig ACL /cfg/HRP/3/acl.conf NEVER


/cfg/HRP/3/acl.conf

SSLAccessControl allow "URL:^/.*" "*:"
	
SSLAccessControl allow "URL:^http://192.168.190.7/ClientUpdate.*" "*:"
	
		
SSLAccessControl allow "IP:10.0.100.2" "CN:SSLTest6 666666666666666666"
	
SSLAccessControl allow "IP:10.0.100.2" "CN:Tom 111111111111111111"
	
SSLAccessControl allow "IP:10.0.100.2" "IDCARD:333333333333333333"
	SSLAccessControl allow "URL:^http://192.168.190.7/ClientUpdate/.*" "*"
SSLAccessControl allow "URL:^/.*" "*"
```

## 1. mod_ssl_acl_tbsg.c
```c 
  /* ACL 模块配置 */
  module AP_MODULE_DECLARE_DATA ssl_acl_tbsg_module = {
      STANDARD20_MODULE_STUFF,
      NULL,            /* create per-directory config structure */
      NULL,            /* merge per-directory config structures */
      create_acl_tbsg_server_config,    /* create per-server config structure */
      NULL,            /* merge per-server config structures */
      acl_tbsg_cmds,            /* command apr_table_t */
      register_hooks        /* register hooks */
  };

```


## 2. acl_tbsg_config.c
```c 
  /* 创建每个 VirtualHost 的配置结构体 */
  void *create_acl_tbsg_server_config(apr_pool_t *p, server_rec *s)
  {
      apr_pool_t *pool = s->process->pool;
      AclTbsgModConfig *mc;

      mc = (AclTbsgModConfig *)apr_palloc(pool, sizeof(*mc));

      mc->pPool = pool;
      /* 是否启用 ACL */
      mc->c_ACL.config.enable = 0;
      /* ACL 规则的更新频率 */
      mc->c_ACL.config.updateFreq = SSL_FREQ_NEVER;
      /* ACL 上次更新时间 */
      mc->c_ACL.config.lastModify = 0;
      /* ACL 配置文件路径 */
      mc->c_ACL.config.file = NULL;
      mc->c_ACL.aACLs = apr_array_make(pool, 1024, sizeof(TBSGACL));
      return mc;

  }
```

## 3. mod_ssl_acl_tbsg.c
```c
static void register_hooks(apr_pool_t *p)
{

    ap_hook_pre_config(tbsg_pre_config, NULL,NULL,APR_HOOK_MIDDLE);

    ap_hook_pre_connection(tbsg_hook_pre_connection,NULL,NULL, APR_HOOK_MIDDLE);

    /**
     *  tbsg-sp2
     *  访问控制功能，访问控制中策略中必须添加证书SSLTest5的用户有访问权限才可验证过,
	 *  ap_hook_access_checker(tbsg_hook_Access,  NULL,NULL, APR_HOOK_MIDDLE);
     */
	KLSSL_hook_acl_check(tbsg_hook_acl_check, NULL, NULL, APR_HOOK_MIDDLE);

	KLSSL_hook_post_handshake(tbsg_hook_post_handshake, NULL, NULL, APR_HOOK_MIDDLE);

}

```

## 4. mod_ssl_acl_tbsg.c
```c
static int tbsg_pre_config(apr_pool_t *p, apr_pool_t *plog, apr_pool_t *ptemp)
{
    KLSSL_register_ext_config_handler(p, "ACL", tbsg_parse_acl_cfg);
    return OK;
}
#+END_SRC

## 5. mod_ssl_acl_tbsg.c
```c
const char *tbsg_parse_acl_cfg( server_rec *server,
		const char *szFileName,
		int updateFreq)
{
	AclTbsgModConfig *mc = TbsgModConfig(server);

	if (mc == NULL) {
		return "Please Enable ssl_acl_tbsg module first";
	}

	//每次解析时先清除原有配置
	KLSSL_log( server, APLOG_INFO, "clear ExtendConfig ACL");
	tbsg_clear_ACLs(&mc->c_ACL);
	mc->c_ACL.config.enable = 1;
	mc->c_ACL.config.updateFreq = updateFreq;
	mc->c_ACL.config.file = szFileName;

	return tbsg_parse_ACLFile( server, szFileName );
}

```


## 6. acl_tbsg_config.c
```c
  const char *tbsg_parse_ACLFile( server_rec *server,
          const char *szFileName )
  {
      AclTbsgModConfig *mc = TbsgModConfig(server);
      TbsgACLConfig *ac = &(mc->c_ACL);

      int rv = 0;
      FILE *fp = NULL;
      const char *szRet = NULL;

      struct stat fs = {0};
      if( 0 == stat( szFileName, &fs ) ) {
          ac->config.lastModify = fs.st_mtime;
      }

      if( fp = fopen(szFileName, "rt") ) {
          char szLine[1024] = {0};
          char szCommand[32] = {0};
          char szAction[32] = {0};
          char szResource[256] = {0};
          char szRole[256] = {0};
          char szTime[256] = {0};

          /*
             按行更新ACL列表，这里可能的行输入为:
             SSLAccessControl allow "URL:https://www.hotmail.com/.*" "BSG_GROUP:000000000000"
             ,*/
          while( fgets(szLine, sizeof(szLine)-1, fp) ) {

              rv = KLSSL_sscanf(szLine, "%s %s %s %s %s", szCommand, szAction, szResource, szRole, szTime);
              if( rv == 4 ){
                /* 如果没有时间这一项,就默认全天都可以访问 */
                  strcpy(szTime,"DAY:00:00:00-23:59:59");
                  KLSSL_log( server, APLOG_INFO ,"ACL No time control parameters can be found,Using default parameters: %s", szTime);
              }
              if( (rv == 5 || rv == 4) && 0 == strcasecmp(szCommand, "SSLAccessControl") ) {
                /* 解析单条ACL规则 */
                  if( szRet = tbsg_parse_ACL( server, szAction, szResource, szRole, szTime, ac ) ) {
                      fclose(fp);
                      return szRet;
                  }
              }
          }

          fclose(fp);
      }
      else {
          KLSSL_log( server, APLOG_ERR,
                  "Can't open ACL file %s", szFileName );
          return "Can't open ACL file";
      }

      return NULL;
  }
```c

## 7. acl_tbsg_config.c
```c
  /* 解析单条ACL规则 */
  const char *tbsg_parse_ACL(	server_rec *server,
          char *szAction,
          char *szResource,
          char *szRole,
          char *szTime,
          TbsgACLConfig *ac )
  {
      TBSGACL temp, *pACL = NULL;

      /*	确定动作	*/
      if( 0 == strncasecmp("allow", szAction, 5) ) {
          /*	允许	*/
          temp.nAction = TBSG_ACTION_ALLOW;
      }
      else if( 0 == strncasecmp("deny", szAction, 4) ) {
          /*	阻止	*/
          temp.nAction = TBSG_ACTION_DENY;
      }
      else {
          KLSSL_log( server, APLOG_ERR, "Invalid acl-action %s", szAction );
          return "Invalid acl-action";
      }

      /*	确定资源	*/
      if( 0 == strncasecmp( szResource, "*", 1 ) || 0 == strncasecmp( szResource, "ALL", 3 ) ) {
          /*	所有资源	*/
          temp.res_type = TBSG_RESOURCE_ALL;
      }
      else if( 0 == strncasecmp( szResource, "URL:", 4 ) ) {
          /*	URL资源					*/
          szResource += 4;
          temp.res_type = TBSG_RESOURCE_URL;
          temp.res_regx = (regex_t*)malloc(sizeof(regex_t));

          if( regcomp( temp.res_regx, szResource, REG_EXTENDED|REG_ICASE ) ) {
              KLSSL_log( server, APLOG_ERR, "pre compile for url-resource %s failed", szResource );
              return "pre compile for url-resource failed";
          }
      }
      else if( 0 == strncasecmp( szResource, "IP:", 3 ) ) {
          /*	IP地址资源	*/
          temp.res_type = TBSG_RESOURCE_IPADDR;
          temp.res_regx = (regex_t*)malloc(sizeof(regex_t));

          if( regcomp( temp.res_regx, szResource + 3, REG_EXTENDED ) ) {
              KLSSL_log( server, APLOG_ERR, "pre compile for ip-resource %s failed", szResource );
              return "pre compile for ip-resource failed";
          }
      }
      else if( 0 == strncasecmp( szResource, "BSG:", 4 ) ) {
          /*
             BSG资源，是使用空格分隔的URL和IP正则表达式，形如:
             "BSG:http://www.test.com/* 10.0.*.*"

             该组合表达，在用于ALLOW规则时表示为“与”，用于DENY规则时忽略IP地址规则
             ,*/
      }
      else {
          KLSSL_log( server, APLOG_ERR, "Invalid acl-resource %s", szResource );
          return "Invalid acl-resource";
      }


      /*	确定角色	*/
      if( 0 == strncasecmp( szRole, "*", 1 ) || 0 == strncasecmp( szRole, "ALL", 3 ) ) {
          /*	所有角色	*/
          temp.role_type = TBSG_ROLE_ALL;
      }
      else if( 0 == strncasecmp( szRole, "IDCARD:", 7) ) {
          /*	公安证书身份证号，必须精确匹配 */
          temp.role_type = TBSG_ROLE_IDCARD;
          temp.role_value = strdup( szRole + 7);
      }
      else if( 0 == strncasecmp( szRole, "CN:", 3 ) ) {
          /*	证书CN项，必须精确匹配	*/
          temp.role_type = TBSG_ROLE_CN;
          temp.role_value = strdup( szRole + 3 );
      }
      else {
          KLSSL_log( server, APLOG_ERR, "Invalid acl-role %s", szRole );
          return "Invalid acl-role";
      }

      /* 确定时间 */
      if( 0 == strncasecmp( szTime, "DAY:", 4 ) ){
          temp.time_type = TBSG_TIME_DAY;
          // timestr = 00:00:00-23:59:59
          temp.timestr = strdup( szTime + 4 );
          KLSSL_log( server, APLOG_DEBUG, "init acl-time string %s",temp.timestr);
      }else{
          KLSSL_log( server, APLOG_INFO, "Invalid acl-time %s",szTime);
          return "Invalid acl-time";
      }

      /* 将此条规则压入数组中 */
      pACL = apr_array_push(ac->aACLs);
      memcpy( pACL, &temp, sizeof(temp) );

      return NULL;
  }

```


## 8. mod_ssl_acl_tbsg.c
```c
static int tbsg_hook_pre_connection(conn_rec *c, void *csd)
{
	AclTbsgConnRec *tbsgconn = TbsgConnConfig(c);

	AclTbsgModConfig *mc = TbsgModConfig(c->base_server);
	if( mc->c_ACL.config.enable && mc->c_ACL.config.updateFreq == SSL_FREQ_PERCONN ) {
		ap_log_error(APLOG_MARK, APLOG_DEBUG , 0, c->base_server,
                         			" tbsg_hook_pre_connection tbsg_check_ACLUpdate");
	    /*	检查ACL是否更新,如果更新了,就重新读取 ACL 规则文件	*/
		tbsg_check_ACLUpdate(c->base_server);
	}
	return APR_SUCCESS;

}
```


## 9. mod_ssl_acl_tbsg.c
```c
      /* 检查ACL配置文件是否已经更新，更新后则重新读取并解析 */
  int tbsg_check_ACLUpdate( server_rec *server )
  {
      AclTbsgModConfig *mc = TbsgModConfig(server);
      TbsgACLConfig *ac = &(mc->c_ACL);

      time_t mtime = 0;

      /*	判断 ACL 配置文件是否已经更新	*/
      if( (mtime = KLSSL_get_LastUpdate(server, &(ac->config))) > ac->config.lastModify ) {
        /* ACL 规则文件已经被更新,重新解析 */
          tbsg_clear_ACLs(ac);
          tbsg_parse_ACLFile( server, ac->config.file );
      }
      return 0;
  }

```

## 10. mod_ssl_acl_tbsg.c
```c
  static int tbsg_hook_acl_check(request_rec *r)
  {
      ap_log_error(APLOG_MARK, APLOG_DEBUG, 0, r->server, "in hook acl");

      AclTbsgModConfig *mc = TbsgModConfig(r->server);
      TbsgACLConfig *ac = &(mc->c_ACL);
      SSLConnRec *sslconn = SSLConnConfig(r->connection);

      if ( ac->config.enable == 0 ) {
          ap_log_error(APLOG_MARK, APLOG_DEBUG, 0, r->server,
                      "ACL DISABLE");
          return 1;
      }


      char *szResource = r->unparsed_uri;
      char szDesc[256] = {0};

      /* 如果更新频率设置为每个请求,则对于每个请求都检查一次 ACL 规则文件 */
      if( ac->config.updateFreq == SSL_FREQ_PERREQUEST ) {
          tbsg_check_ACLUpdate(r->server);
      }

      int rv = tbsg_check_ACL(r, szResource, sslconn->client_cert, ac, szDesc);
      /* 给 request_rec 的 notes 表正添加一项 UserCertInfo, 用于向下一个模块传递信息 */
      apr_table_setn(r->notes, "UserCertInfo", szDesc);
      if( TBSG_ACTION_OF(rv) == TBSG_ACTION_DENY ) {
          return TBSG_ACTION_DENY;
      }
      else {
          return TBSG_ACTION_ALLOW;
      }
  }

```

## 11. acl_tbsg_kernel.c
```c
  /* ACL检查，不同的资源和角色类型有不同的检查方法 */
  int tbsg_check_ACL(	request_rec *r, char *szResource, X509* pCert, 
          TbsgACLConfig *ac, char* szDesc)
  {

      AclTbsgModConfig *mc = TbsgModConfig(r->server);

      SSLConnRec *sslconn = SSLConnConfig(r->connection);
      SSLModConfigRec *smc = SSLModConfig(r->server);

      /*如果不开启 ACL 规则检查, 则默认允许此次连接 */
      if ( !ac->config.enable ) {
          return TBSG_ACTION_ALLOW;
      }

      int i = 0;
      TBSGACL *acls = (TBSGACL *)(ac->aACLs->elts);
      const char *szRole = NULL;
      char szVar[16] = {0};
      char szIP[16] = {0};

      /* 判断r->parsed_uri.hostname是否是IP地址 */
      regex_t *preg = NULL;

      preg = ap_pregcomp(r->pool, szTmpIp, REG_EXTENDED );

      if( ap_regexec(preg, r->hostname, 0, NULL, 0) ){
          if( !r->parsed_uri.hostname ) {

              if( apr_uri_parse( r->pool, szResource, &r->parsed_uri ) ) {
                  ap_log_error(APLOG_MARK, APLOG_ERR, 0, r->server,
                      "ACL Rule : apr_uri_parse for %s failed", szResource );
                      return TBSG_ACTION_DENY;
              }
          }
          r->parsed_uri.hostent = gethostbyname(r->parsed_uri.hostname);
          /* 获取访问者的 IP 地址 */
          apr_snprintf( szIP, 16, "%u.%u.%u.%u", 
                      (unsigned char)r->parsed_uri.hostent->h_addr[0],
                      (unsigned char)r->parsed_uri.hostent->h_addr[1],
                      (unsigned char)r->parsed_uri.hostent->h_addr[2],
                      (unsigned char)r->parsed_uri.hostent->h_addr[3] );
      }else{
          apr_snprintf(szIP,"%s",r->hostname);
      }

      if( !szIP ) {
          ap_log_error(APLOG_MARK, APLOG_ERR, 0, r->server,
              "ACL Rule :  resolv %s address failed", szResource);
              return TBSG_ACTION_DENY;
      }

      /*	进行ACL规则检查	*/
      for (i = 0; i < ac->aACLs->nelts; i++) {
          int nActionMethod = TBSG_ACTION_METHOD_OF(acls[i].nAction);

          switch( acls[i].res_type ) {
              case TBSG_RESOURCE_ALL:
                  ap_log_error(APLOG_MARK, APLOG_DEBUG, 0, r->server,
                          "ACL Rule %d: %s matched resource *", i, szResource );
                  break;
              case TBSG_RESOURCE_URL:
                  if ( nActionMethod && nActionMethod != (r->method_number + TBSG_ACTION_METHOD_BASE)) {
                      ap_log_error(APLOG_MARK, APLOG_DEBUG, 0, r->server,
                              "ACL Rule %d: method %s didn't match URL resource regex", 
                              i, r->method );
                      continue;
                  }
                  if( ap_regexec(acls[i].res_regx, szResource, 0, NULL, 0) ) {
                      ap_log_error(APLOG_MARK, APLOG_DEBUG, 0, r->server,
                              "ACL Rule %d: %s didn't match URL resource regex", i, szResource );
                      continue;
                  }
                  else {
                      ap_log_error(APLOG_MARK, APLOG_DEBUG, 0, r->server,
                              "ACL Rule %d: %s matched URL resource regex", i, szResource );
                  }
                  break;
              case TBSG_RESOURCE_IPADDR: 

                  //2007-4-30 yanhm bugfix for 0001871 +{{
                  //反向代理时,IP类型的资源描述不支持,直接跳过此策略
                  if( SSL_CONN_REVERSE == sslconn->nConnType ) {
                      ap_log_error(APLOG_MARK, APLOG_DEBUG, 0, r->server,
                              "ACL Rule %d: ACL Rule with type of IPAddr is not valid for SSL_CONN_REVERSE", i);
                      continue;
                  }
                  //}}
                  if( ap_regexec(acls[i].res_regx, szIP, 0, NULL, 0) ) {
                      ap_log_error(APLOG_MARK, APLOG_DEBUG, 0, r->server,
                              "ACL Rule %d: %s didn't match IPAddr resource regex", i, szIP);
                      continue;
                  }
                  else {
                      ap_log_error(APLOG_MARK, APLOG_DEBUG, 0, r->server,
                              "ACL Rule %d: %s matched IPAddr resource regex", i, szIP);
                  }
                  break;
              case TBSG_RESOURCE_BSG:
                  }
                  break;
              default:
                  ap_log_error(APLOG_MARK, APLOG_WARNING, 0, r->server,
                          "ACL Rule %d: Unsupported resource type: %d", i, acls[i].res_type );
                  return TBSG_ACTION_DENY;
          }

          switch( acls[i].role_type ) {
              case TBSG_ROLE_ALL:
                  if (pCert == NULL) {
                      strcpy( szDesc, "匿名" );
                  }
                  else {
                      strcpy( szDesc, "DN为" );
                      // strcat( szDesc, KLSSL_get_Subject(r->pool, X509_get_subject_name(pCert), &(smc->encoding_ctx)) );
                      strcat( szDesc, KLSSL_get_Subject(r->pool, X509_get_subject_name(pCert)) );
                  }

                  ap_log_error(APLOG_MARK, APLOG_DEBUG, 0, r->server,
                          "ACL Rule %d: %s matched role *", i, szDesc );
                  break;
              case TBSG_ROLE_SERIAL_DEC:
                  break;
              case TBSG_ROLE_SERIAL_HEX:
                  break;
              case TBSG_ROLE_OU:
                  break;
              case TBSG_ROLE_O:
                  break;
              case TBSG_ROLE_ST:
                  break;
              case TBSG_ROLE_L:
                  break;
              case TBSG_ROLE_CN:
                  if (pCert == NULL) {
                      strcpy( szDesc, "匿名" );
                      ap_log_error(APLOG_MARK, APLOG_ERR, 0, r->server, 
                              "ACL Rule %d: 不提交证书的用户无法进行证书项(CN)相关的ACL检查", i );

                      return TBSG_ACTION_DENY;
                  }

                  strcpy( szVar, "CN" );
                  szRole = ssl_var_lookup_ssl_cert_dn(	r->pool, 
                          X509_get_subject_name(pCert), 
                          szVar);

                  strcpy( szDesc, "CN为" );
                  strcat( szDesc, szRole );

                  if( strcmp( acls[i].role_value, szRole ) ) {
                      ap_log_error(APLOG_MARK, APLOG_DEBUG, 0, r->server,
                              "ACL Rule %d: %s didn't match ROLE_CN %s", i, szRole, acls[i].role_value );
                      continue;
                  }
                  else{
                      ap_log_error(APLOG_MARK, APLOG_DEBUG, 0, r->server,
                              "ACL Rule %d: %s matched ROLE_CN %s", i, szRole, acls[i].role_value );
                  }

                  break;
              case TBSG_ROLE_BSGGROUP:
                  break;
              case TBSG_ROLE_IDCARD:
                  if ( pCert == NULL ) {
                      strcpy( szDesc, "匿名" );
                      ap_log_error(APLOG_MARK, APLOG_ERR, 0, r->server, 
                              "ACL Rule %d: 不提交证书的用户无法对证书身份证号(CLIENT_IDCARD)进行相关的ACL检查", i );
                      return TBSG_ACTION_DENY;
                  }
                  szRole = ssl_var_lookup( r->pool, r->server, r->connection, r, "GA_CLIENT_IDENTITY" );

                  strcpy( szDesc, "身份证号为" );
                  strcat( szDesc, szRole );

                  if( strcmp(acls[i].role_value, szRole) ){
                      ap_log_error(APLOG_MARK, APLOG_DEBUG, 0, r->server,
                              "ACL Rule %d: %s didn't match CLIENT_IDCARD %s", i, szRole, acls[i].role_value );
                      continue;
                  } else {
                      ap_log_error(APLOG_MARK, APLOG_DEBUG, 0, r->server,
                              "ACL Rule %d: %s matched CLIENT_IDCARD %s", i, szRole, acls[i].role_value );
                  }
                  break;
              default:
                  ap_log_error(APLOG_MARK, APLOG_WARNING, 0, r->server,
                          "ACL Rule %d: Unsupported role type: %d", i, acls[i].role_type );
                  return TBSG_ACTION_DENY;
          }

          if( TBSG_ACTION_OF(acls[i].nAction) == TBSG_ACTION_DENY ) { 
              ap_log_error(APLOG_MARK, APLOG_INFO, 0, r->server,
                      "ACL Rule %d: DENY ACL rule found for %s to %s %s", 
                      i, szDesc, r->method, szResource );
          }
          else {
              ap_log_error(APLOG_MARK, APLOG_DEBUG, 0, r->server,
                      "ACL Rule %d: ALLOW ACL rule found for %s to %s %s", 
                      i, szDesc, r->method, szResource );
          }

          return acls[i].nAction;
      }

      if (pCert == NULL) {
          strcpy( szDesc, "匿名" );
      }
      else {
          strcpy( szDesc, "DN为" );
          strcat( szDesc, KLSSL_get_Subject(r->pool, X509_get_subject_name(pCert)) );
      }

      ap_log_error(APLOG_MARK, APLOG_WARNING, 0, r->server,
              "No ACL Rrule found for %s to %s %s, return default action %d", 
              szDesc, 
              r->method,
              szResource, 
              TBSG_ACTION_DENY );

      return TBSG_ACTION_DENY;
  }

```
