package org.komodo.rest;
/*
 * JBoss, Home of Professional Open Source.
* See the COPYRIGHT.txt file distributed with this work for information
* regarding copyright ownership. Some portions may be licensed
* to Red Hat, Inc. under one or more contributor license agreements.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
*/


import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.komodo.rest.json.LinkSerializerTest;
import org.komodo.rest.relational.RestConnectionTest;
import org.komodo.rest.relational.RestDataserviceTest;
import org.komodo.rest.relational.RestVdbDataRoleTest;
import org.komodo.rest.relational.RestVdbImportTest;
import org.komodo.rest.relational.RestVdbPermissionTest;
import org.komodo.rest.relational.RestVdbTest;
import org.komodo.rest.relational.RestVdbTranslatorTest;
import org.komodo.rest.relational.json.DataserviceSerializerTest;
import org.komodo.rest.relational.json.QueryResultSerializerTest;
import org.komodo.rest.relational.json.VdbDataRoleSerializerTest;
import org.komodo.rest.relational.json.VdbImportSerializerTest;
import org.komodo.rest.relational.json.VdbPermissionSerializerTest;
import org.komodo.rest.relational.json.VdbSerializerTest;
import org.komodo.rest.relational.json.VdbTranslatorSerializerTest;
import org.komodo.rest.service.KomodoDataserviceServiceTest;
import org.komodo.rest.service.KomodoConnectionServiceTest;
import org.komodo.rest.service.KomodoDriverServiceTest;
import org.komodo.rest.service.KomodoImportExportServiceTest;
import org.komodo.rest.service.KomodoSearchServiceTest;
import org.komodo.rest.service.KomodoUtilServiceTest;
import org.komodo.rest.service.KomodoVdbServiceTest;

@SuppressWarnings( "javadoc" )
@RunWith( Suite.class )
@Suite.SuiteClasses( {
        RestLinkTest.class,
        LinkSerializerTest.class,

        RestDataserviceTest.class,
        RestConnectionTest.class,
        RestVdbDataRoleTest.class,
        RestVdbImportTest.class,
        RestVdbPermissionTest.class,
        RestVdbTest.class,
        RestVdbTranslatorTest.class,
        QueryResultSerializerTest.class,

        DataserviceSerializerTest.class,
        VdbDataRoleSerializerTest.class,
        VdbImportSerializerTest.class,
        VdbPermissionSerializerTest.class,
        VdbSerializerTest.class,
        VdbTranslatorSerializerTest.class,

        KomodoDataserviceServiceTest.class,
        KomodoConnectionServiceTest.class,
        KomodoDriverServiceTest.class,
        KomodoImportExportServiceTest.class,
        KomodoSearchServiceTest.class,
        KomodoUtilServiceTest.class,
        KomodoVdbServiceTest.class
    } )
public class AllTests {
    // nothing to do
}
