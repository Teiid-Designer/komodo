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
package org.komodo.shell.commands;

import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.komodo.shell.AbstractCommandTest;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * Test Class to test setting WorkspaceStatus global and provider properties
 */
@SuppressWarnings( { "javadoc", "nls" } )
public class WorkspaceStatusPropertyTest extends AbstractCommandTest {

    @Test
    public void shouldSetGlobalProperty() throws Exception {
        // Test default
        assertEquals( "true" , wsStatus.getGlobalProperties().getProperty(WorkspaceStatus.SHOW_TYPE_IN_PROMPT_KEY));
        
        // Set a property
        wsStatus.setGlobalProperty(WorkspaceStatus.SHOW_TYPE_IN_PROMPT_KEY, "false");
        
        // Test property changed
        assertEquals( "false" , wsStatus.getGlobalProperties().getProperty(WorkspaceStatus.SHOW_TYPE_IN_PROMPT_KEY));
    }
    
    @Test
    public void shouldSetProvidedProperty() throws Exception {
        // Test no provided properties
        assertEquals( 0 , wsStatus.getProvidedProperties().size() );
        
        // Set a property
        wsStatus.setProvidedProperty("myPropKey", "aValue");
        
        // Test property was set
        assertEquals( "aValue" , wsStatus.getProvidedProperties().getProperty("myPropKey") );
    }
    
    @Test
    public void shouldTestGlobalAfterAddingProvided() throws Exception {
        // Number of global at startup
        int startingGlobal = wsStatus.getGlobalProperties().size();
        
        // Test no provided properties
        assertEquals( 0 , wsStatus.getProvidedProperties().size() );
        
        // Set provided properties
        wsStatus.setProvidedProperty("myPropKey1", "aValue1");
        wsStatus.setProvidedProperty("myPropKey2", "aValue2");
        wsStatus.setProvidedProperty("myPropKey3", "aValue3");
        
        // Count global after adding provided properties
        assertEquals( startingGlobal , wsStatus.getGlobalProperties().size() );
        // Count provided after adding provided properties
        assertEquals( 3 , wsStatus.getProvidedProperties().size() );
        
        // Ensure global does not contain provided properties
        assertEquals( "aValue1", wsStatus.getProvidedProperties().getProperty("myPropKey1"));
        assertEquals( "aValue2", wsStatus.getProvidedProperties().getProperty("myPropKey2"));
        assertEquals( "aValue3", wsStatus.getProvidedProperties().getProperty("myPropKey3"));
    }

    @Test
    public void shouldTestProvidedAfterSettingGlobal() throws Exception {
        // Number of provided at startup
        int startingProvided = wsStatus.getProvidedProperties().size();
        // Number of global at startup
        int startingGlobal = wsStatus.getGlobalProperties().size();
        
        // Set global properties
        wsStatus.setGlobalProperty(WorkspaceStatus.SHOW_TYPE_IN_PROMPT_KEY, "false");
        wsStatus.setGlobalProperty(WorkspaceStatus.AUTO_COMMIT, "false");
        wsStatus.setGlobalProperty(WorkspaceStatus.EXPORT_DEFAULT_DIR_KEY, "exportDefaultDir");
        
        // Number of global doesnt change - they all have defaults
        assertEquals( startingGlobal , wsStatus.getGlobalProperties().size() );
        // Number of provided does not change
        assertEquals( startingProvided , wsStatus.getProvidedProperties().size() );
    }

}
