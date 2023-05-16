# abap-ca-ldap

En este paquete contiene clase y programa de ejemplo para hacer consultas a LDAP.

Este proyecto nace de un proyecto de creación de usuarios LDAP el cual necesitaba conectarse a LDAP para la recuperación de determinados valores: grupos de cloud de SAP, email, etc..  Nacio para un cliente y un proyecto pero
la clase de acceso a LDAP la hice cross con la idea de mejorar y ampliarla en otros proyectos. O hacerle más parametrizable a medida que se evolucione.

Este paquete se hizo en un SAP aparte con GRC instalado que tiene acceso al LDAP, cosa, que otra SAP no lo tienen.

# Clase(s)

La clase principal es la ZCL_LDAP_QUERY. Esta clase tenemos los siguientes métodos principales, los métodos pueden ir variando a medida que se evoluciona la clase.

* QUERY_GENERIC -> Método generico para hacer consultas al LDAP. Es el método base donde llamarán el resto de métodos.
* GET_ACCOUNTNAME_FROM_USERMAIL -> Método que encapsula el método QUERY_GENERIC para poder obtener el código de usuario a partir del email.
* GET_ACCOUNTNAME_INFO -> Método que devuelve información general del usuario

# Programas

Para testear y ver el funcionamiento de la clase ZCL_LDAP_QUERY hay dos programas:

* ZLDAP_R_TEST_ACCOUNTNAME -> Devuelve el usuario asignado en el LDAP a partir del email
* ZLDAP_R_TEST_USERINFO -> Devuelve información general del usuario

# Funciones

Se creo el grupo de funciones ZLDAD_RFC para poder atacar a las clases a través de RFC. Se hizo porque como se ha dicho al principio estas clases se han hecho en un SAP con GRC, independiente de otros sistemas SAP del cliente.

Las funciones RFC habilitadas son:

* ZCA_RFC_USER_CREATED_BY_EMAIL -> Devuelve el usuario asignado el email pasado por parámetro.

