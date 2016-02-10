/*
 * Copyright 2014 JBoss Inc
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.komodo.relational.commands;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import java.util.Arrays;
import org.komodo.relational.model.OptionContainer;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.KException;

/**
 *
 */
public class CustomOptionCommandTest extends AbstractCommandTest{

    /**
     * Returns a VDB with the specified name
     *
     * @param vdbName Name of the VDB to return
     * @return Vdb object or null if there is no VDB with the name
     * @throws KException
     */
    protected Vdb getVDB(String vdbName, WorkspaceManager wkspMgr) throws KException{
        Vdb[] vdbs=wkspMgr.findVdbs(getTransaction());
        for(Vdb vdb: vdbs){
            if(vdb.getName(getTransaction()).equals(vdbName)){
                return vdb;
            }
        }
        return null;
    }

    /**
     * Asserts options commands
     * @param kobject OptionContainer object in which are located the options
     * @param expectedOptions expectedOptions
     * @throws KException
     */
    protected void assertCustomOptions(OptionContainer kobject,
                                       String[][] expectedOptions) throws KException {
        StatementOption[] options = kobject.getCustomOptions(getTransaction());
        int counter = 0;
        String[][] actualOptions = new String[options.length][2];
        for (StatementOption option : options) {
            actualOptions[counter][0] = option.getName(getTransaction());
            actualOptions[counter][1] = option.getStringValue(getTransaction());
            counter++;
        }
        String compareMessage = "Expected: " + Arrays.deepToString(expectedOptions) + " Actual: " //$NON-NLS-1$//$NON-NLS-2$
                                + Arrays.deepToString(actualOptions);
        assertThat("Invalid number of option keys " + compareMessage, actualOptions.length, is(expectedOptions.length)); //$NON-NLS-1$
        assertThat("Invalid elements: " + compareMessage, Arrays.deepEquals(expectedOptions, actualOptions), is(true)); //$NON-NLS-1$

    }
}
