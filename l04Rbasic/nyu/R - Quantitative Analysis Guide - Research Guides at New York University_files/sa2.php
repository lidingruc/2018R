
                var COOKIE_SESSION_NAME = "sts";
                var COOKIE_SESSION_END_NAME = "stse";
                var COOKIE_REFERRER_NAME = "str";
                var TRACKING_MODE_ONCLICK = 1;
                var TRACKING_TYPE_PAGE = 1;
                var TRACKING_TYPE_WIDGET = 9;
                var TRACKING_TYPE_API = 10;
                var TRACKING_TYPE_LINK = 2;
                var TRACKING_TYPE_BOOK = 3;
                var TRACKING_TYPE_EVENT = 4;
                var TRACKING_TYPE_FILE = 5;
                var TRACKING_TYPE_FORM_INPUT = 6;
                var TRACKING_TYPE_FORM_BUTTON = 7;
                var TRACKING_TYPE_FORM = 8;
                var TRACKING_TYPE_HOME_PAGE = 11;
                var TRACKING_TYPE_CUSTOM_LINK = 12;
                var TRACKING_TYPE_SEARCH = 13;
                var TRACKING_TYPE_HOME_PAGE_MOBILE = 14;
                var TRACKING_TYPE_PAGE_MOBILE = 15;
				var TRACKING_TYPE_SEARCH_AZ = 27;
                
				// make sure we have a springSpace object defined
				var springSpace = springSpace || {};
				
                springSpace.springTrack = springSpace.springTrack || {
                    site_id: 873,
                    st_prefix: "_st_",
                    // Springshare data (optional)
                    // This will be populated with any config properties of the form "_st_<id_name>".
                    st_tracking_parameters: {},
                    user_logged_in: false,
                    is_logout: ( document.location.search.indexOf("msg=logout") > -1 ),
                    do_not_track: ( document.location.search.indexOf("tracking=off") > -1 ),
                    is_preview: ( document.location.search.indexOf("preview=") > -1 ),
                    is_cg: true,
                    tracking_parameters: {},
                    tracking_server_host: "libguides-proc.springyaws.com",
                    tracking_script: "log.php",
                    page_params: document.location.search,
                    opt_use_page_params: true,
                    imgs: new Array(),
                    img_idx: 0,
                    track_delay: 600,
                    sessionCookieTimeout: 30, // 30 minutes
                    referrerCookieTimeout: 60 * 24 * 30 * 6, // 6-months
                    cookie_session: {},
                    cookie_referrer: {},
                    new_session: false,
                    tracking_fields: {
                                "site_path" : {"key" : "sp", "val" : document.location.pathname + ( document.location.search.length > 0 ? document.location.search : "" ) },
                                "resource_type" : {"key" : "rt", "val" : "" },
                                "resource_id" : {"key" : "rid", "val" : "" },
                                "search_terms" : {"key" : "st", "val" : "" },
                                "custom_data" : {"key" : "cd", "val" : "" },
                                "monitor_resolution" : {"key" : "mr", "val" : screen.width + "x" + screen.height },
                                "cookie_session_start" : {"key" : "sst", "val" : "" },
                                "cookie_session_id" : {"key" : "sid", "val" : "" },
                                "cookie_referrer" : {"key" : "rr", "val" : "" },
                                "visitor_id" : {"key" : "vid", "val" : "" }
                            },
                    // Cookie processing.
                    //this.processCookies();
                    processCookies: function() {
                        //debugger;
                        // Check for existing cookie.
                        this.cookie_session = this.readCookie(COOKIE_SESSION_NAME);
                        this.cookie_referrer = this.readCookie(COOKIE_REFERRER_NAME);
                        var unique_id = "873_58e35da13895f";
                        var date = new Date();
                        
                        if ( this.cookie_session == null || this.cookie_session.length == 0 ) {
                            var session_data = "{id:'" + unique_id + "',start:'" + date.getTime() + "'}";
                            this.createCookie(COOKIE_SESSION_NAME, session_data, this.sessionCookieTimeout);
                            
                            // Set cookie variable.
                            this.cookie_session = session_data;
                            
                            // Set new session flag.
                            this.new_session = true;
                        } else {
                            // Update cookie expiration.
                            this.updateCookieExpiration(COOKIE_SESSION_NAME, this.sessionCookieTimeout);
                        }
                        
                        if ( this.cookie_referrer == null || this.cookie_referrer.length == 0 ) {
                            //debugger;
                            var referrer_data = "{id:'" + unique_id + "',ref:'" + encodeURIComponent(document.referrer) + "'}";
                            this.createCookie(COOKIE_REFERRER_NAME, referrer_data, this.referrerCookieTimeout);
                            
                            this.cookie_referrer = referrer_data;
                        } else {
                            // Update cookie value.
                            //debugger;
                            var referrer_data = eval( "(" + this.cookie_referrer + ")");
                            
                            // Persist existing id and set a new referrer.
                            referrer_data = "{id:'" + referrer_data.id + "',ref:'" + encodeURIComponent(document.referrer) + "'}";
                            
                            this.createCookie(COOKIE_REFERRER_NAME, referrer_data, this.referrerCookieTimeout);
                        }
                    },
                    createCookie: function(name,value,expires_min) {
                        if (expires_min) {
                            var date = new Date();
                            // 24*60*
                            date.setTime(date.getTime()+(expires_min*60*1000));
                            var expires = "; expires="+date.toGMTString();
                        }
                        else var expires = "";
                        document.cookie = name+"="+value+expires+"; path=/";
                    },
                    readCookie: function(name) {
                        var nameEQ = name + "=";
                        var ca = document.cookie.split(";");
                        for(var i=0;i < ca.length;i++) {
                            var c = ca[i];
                            while (c.charAt(0)==" ") c = c.substring(1,c.length);
                            if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
                        }
                        return null;
                    },
                    updateCookieExpiration: function(name, expires_min) {
                        // Create a new cookie with the same value and a new expiration time.
                        this.createCookie(name,this.readCookie(name),expires_min);
                    },
                    eraseCookie: function(name) {
                        this.createCookie(name,"",-1);
                    },
                    init: function(config_obj) {
                        // This will allow us to set parameters at the top of a pgae, then use them for subsequent calls
                        // that may not necessarily have access to certain data.
                        this.tracking_parameters = config_obj;
                    },
                    setSpringTrackData: function(config_obj) {
                        // Loop over the config elements and add any of the form "_st_<id>" to the st_tracking_objects.
                        //debugger;
                        for ( elt in config_obj ) {
                            if ( elt.indexOf(this.st_prefix) == 0 ) {
                                this.st_tracking_parameters[elt] = config_obj[elt];
                            }
                        }
                    },
                    setParameter: function(name, value, label) {
                        // Use this to set tracking parameters to send to the tracking server.
                        
                    },
                    trackIt: function(config) {
                        // This method is responsible for generating the tracking data request to the tracking server.
                        //this.setSpringTrackData(config);
                        
                        // If this is a logged-in user dont log.
                        // If this is a preview dont log
                        // If this is a logout dont log.
                        // If this is a do not track dont log.
                        if ( !this.user_logged_in && !this.is_preview && !this.is_logout && !this.do_not_track ) {
                            var server_path;
                            
                            switch (config._st_log_type) {
                                case "1":
                                    server_path = this.tracking_server_host;
                                    break;
                                case "2":
                                    server_path = "guides.nyu.edu/libguides";
                                    break;
                                default:
                                    server_path = this.tracking_server_host;
                                    break;
                            }
							
                            var track_src = "//" + server_path + "/" + this.tracking_script + this.generateQSData(config);
                            
                            if (document.images){
                                this.imgs[this.img_idx] = new Image;
                                this.imgs[this.img_idx].src = track_src;
                                this.img_idx++;
                            }
                            else{
                                document.write('<img alt="Tracking" src="' + track_src + '" style="height: 0px; width: 0px; display: none;" />');
                            }
                        }
                        
                        return true;
                    },                    
                    generateQSData: function(config) {
                        // This method is responsible for generating the tracking data request to the tracking server.
                        var qs_arr = new Array();
                        var qs_string = "";
                        
                        // Set cookie tracking fields.
                        this.generateCookieQSData();
                        
                        if ( this.opt_use_page_params ) {
                            var page_parm_arr = this.page_params.split("&");
                        }
                        if ( this.tracking_fields != null ) {
                            for ( field in this.tracking_fields ) {
                                qs_arr.push(this.tracking_fields[field]["key"] + "=" + encodeURIComponent(this.tracking_fields[field]["val"]));
                            }
                        }
                        
                        if ( this.tracking_parameters != null ) {
                            // Make sure we have a site_id, otherwise set it with the class property.
                            var site_id_exists = false;
                            
                            for ( param in this.tracking_parameters ) {
                                if ( param == "_st_site_id" ) {
                                    site_id_exists = true;
                                }
                                qs_arr.push(param + "=" + encodeURIComponent(this.tracking_parameters[param]));
                            }
                            
                            if ( !site_id_exists ) {
                                qs_arr.push("_st_site_id=" + this.site_id);
                            }
                        } else {
                            qs_arr.push("_st_site_id=" + this.site_id);
                        }
                        
                        if ( config != null ) {
                            if ( typeof(config["_full_qs"]) != "undefined" && config["_full_qs"] ) {
                                qs_arr.push("_full_qs=" + encodeURIComponent(document.location.search));
                            } else {
                                for ( param in config ) {
                                    // Ignore the "link" param.
                                    if ( param !== "link" ) {
                                        // Dont double-up parameters that already exist in tracking parameters.
                                        if ( typeof(this.tracking_parameters[param]) == "undefined" ) {
                                            qs_arr.push(param + "=" + encodeURIComponent(config[param]));
                                        }
                                    }
                                }
                            }
                        }
                        
                        var date = new Date();
                        
                        if ( qs_arr.length > 0 ) {
                            return "?action=track&" + qs_arr.join("&") + "&_nocache=" + date.getTime();
                        } else {
                            return "?action=track&nocache=" + date.getTime();
                        }
                    },
                    //-------------------------------------------------------
                    //  Compile cookie data for sending on the querystring.
                    //-------------------------------------------------------
                    generateCookieQSData: function() {
                        var qs_arr = new Array();
                        var session_cookie = eval("(" + this.cookie_session + ")");
                        
                        // If we have a new session send the start time as an indication.
                        if ( this.new_session ) {
                            if ( this.cookie_session != null ) {
                                this.tracking_fields.cookie_session_start.val = session_cookie.start;
                                //qs_arr.push("sst=" + escape(session_cookie.start));
                            }
                        }
                        
                        // Always send the session id.
                        this.tracking_fields.cookie_session_id.val = session_cookie.id;
                        //qs_arr.push("sid=" + escape(session_cookie.id));
                        
                        if ( this.is_cg ) {
                            // Get/send referrer data.
                            referrer_cookie = eval("(" + this.cookie_referrer + ")");
                            this.tracking_fields.cookie_referrer.val = document.referrer; //referrer_cookie.ref;
                            //qs_arr.push("rr=" + escape(referrer_cookie.ref));
                        }
                        
                        //return qs_arr.join("&");
                    },                    
                    propExists: function(obj, prop) {
                        return ( typeof(obj[prop]) != "undefined" ? true : false );
                    },
                    getResourceType: function(elt, config) {
                        // If we have a config object, look for "_st_" and "custom_type" properties, otherwise try to figure out what kind of object has been passed in.
                        var r_type = -1;
                        var type_id = "_st_type_id";
                        
                        //debugger;
                        if ( config != null ) {
                            if ( this.propExists(config, type_id) ) {
                                // TO DO: Rethink how we do the hit source stuff.
                                if ( config[type_id] == "r" ) {
                                    r_type = 1;
                                } else if ( config[type_id] == "w" ) {
                                    r_type = 9;
                                } else if ( config[type_id] == "a" ) {
                                    r_type = 10;
                                } else {
                                    r_type = config[type_id];
                                }
                            } else if (this.propExists(config, "custom_type")) {
                                r_type = config["custom_type"];
                            } else {
                                r_type = 2;	
                            }
                        } else if ( elt != null ) {
                            if ( this.propExists(elt, "tagName") ) {
                                if ( elt.tagName.toUpperCase() == "A" ) {
                                    r_type = 2;
                                } else if ( elt.tagName.toUpperCase() == "INPUT" ) {
                                    r_type = 6;
                                } else if ( elt.tagName.toUpperCase() == "BUTTON" ) {
                                    r_type = 7;
                                } else if ( elt.tagName.toUpperCase() == "FORM" ) {
                                    r_type = 8;
                                } else {
                                    r_type = 1;	
                                }
                            } else {
                                r_type = 1;
                            }
                        } else {
                            r_type = 1;
                        }
                        
                        return r_type;
                    },
                    track: function(elt, config) {
                        //debugger;
                        var tmp_elt = ( typeof(elt) != "undefined" ? elt : document );
                        var elt_tag_name = ( this.propExists(tmp_elt, "tagName") ? tmp_elt.tagName : "" );
                        
                        switch (elt_tag_name) {
                            case "A":
                                this.trackLink(tmp_elt, config);
                                break;
                            case "FORM":
                                this.trackForm(tmp_elt, config);
                                break;
                            case "DOCUMENT":
                                this.trackPage(tmp_elt, config);
                                break;
                            default:
                                this.trackPage(tmp_elt, config);
                        }
                    },
                    gaLinkTracking: function(config_obj) {
                        try {
                            if ( typeof(pageTracker) != "undefined" && typeof(config_obj) != "undefined" ) {
                                var link_name = config_obj.link_obj.innerHTML.replace("&amp;", "&");
                                var link_id = "/" + config_obj.guide_name + "/" + config_obj.page_name + "/" + link_name + "/" + escape(config_obj.link_obj.href);
                                pageTracker._trackPageview(link_id);
                            }
                        } catch (e) {}
                    },
                    trackLink: function(config) {
                        //this.gaLinkTracking();
                        this.tracking_fields.custom_data.val = ( typeof(config.custom_data) !== "undefined" ? config.custom_data : "" );
                        this.trackIt(config);
                        
                        if ( config.link.target ) {
                            return true;
                        } else {
                            setTimeout("document.location.href = '" + config.link.href + "';", this.track_delay);
                            return false;
                        }
                    },
                    trackPage: function(config) {				
                        //this.tracking_fields.resource_type.val = this.getResourceType(null, config);
                        this.trackIt(config);
                    },
                    trackEr: function(config) {				
                        this.trackIt(config);
                    },
                    trackDiscussion: function(config) {                    
                        this.trackIt(config);
                    },
                    trackBlog: function(config) {                    
                        this.trackIt(config);
                    },
                    trackPageRedirect: function(config) {				
                        //this.tracking_fields.resource_type.val = this.getResourceType(null, config);
                        
                        //debugger;
                        this.trackIt(config);
                        
                        if ( this.propExists(config, "mode") && config.mode == TRACKING_MODE_ONCLICK ) {
                            if ( config.target ) {
                                return true;
                            } else {
                                setTimeout("document.location.href = '" + config.href + "';", this.track_delay);
                                return false;
                            }
                        }
                    },
                    trackForm: function(form_obj, config) {
                        var resource_parts = form_obj.action.replace(/^http(s)?:\/\//ig, "").split("/");
                        //var resource_type = this.getResourceType(form_obj, config);
                        //debugger;
                        
                        //this.tracking_fields.resource_type.val = resource_type;
                        //this.tracking_fields.resource_host_name.val = resource_parts[0];
                        //this.tracking_fields.resource_path.val = form_obj.action;
                        //this.tracking_fields.resource_data.val = form_obj.action;
                        
                        this.trackIt(config);
                        
                        setTimeout("form_obj.submit();", this.track_delay);
                        
                        return false;
                    },
                    trackSearch: function(config) {
                        //this.tracking_fields.resource_type.val = this.getResourceType(null, config);
                        this.tracking_fields.search_terms.val = ( typeof(config.search_terms) !== "undefined" ? config.search_terms : "" );
                        this.trackIt(config);
                    }
                }
                
                // Instantiate global tracking object.
                //var springTrack = new springTrack();
                springSpace.springTrack.processCookies()